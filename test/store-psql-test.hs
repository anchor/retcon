--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}


module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Char8      as BS
import           Data.Monoid
import           Data.Text                  ()
import qualified Data.Vector                as V
import           Database.PostgreSQL.Simple
import           System.Process
import           Test.Hspec

import           Retcon.Configuration
import           Retcon.Diff
import           Retcon.Document
import           Retcon.Identifier
import           Retcon.Monad
import           Retcon.Store
import           Retcon.Store.PostgreSQL

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

dbname :: BS.ByteString
dbname = "retcon_test"

options :: StoreOpts PGStore
options = PGOpts { connstr = "dbname=" <> dbname }

config :: Configuration
config = emptyConfiguration

runAction :: RetconMonad a -> IO (Either RetconError a)
runAction = runRetconMonad (RetconEnv config)

-- | Check that a SqlError exception is about a disconnected connection.
isDisconnected :: SqlError -> Bool
isDisconnected e = sqlErrorMsg e == "connection disconnected"


--------------------------------------------------------------------------------

-- | Bunch of data structures to track table contents
type RetconTable        = [(String, Int)]
type RetconFkTable      = [(String, Int, String, String)]
type RetconInitialTable = [(String, Int, Value)]
type RetconDiffs        = [(String, Int, Int, Value)]
type RetconConflicts    = [(Int, Int, Value)]

-- | Count the objects in the store.
countStore :: PGStore -> IO (Int, Int, Int, Int)
countStore (PGStore conn _) = do
    [Only iks] <- query_ conn "SELECT count(*) FROM retcon;"
    [Only fks] <- query_ conn "SELECT count(*) FROM retcon_fk;"
    [Only docs] <- query_ conn "SELECT count(*) FROM retcon_initial;"
    [Only diffs] <- query_ conn "SELECT count(*) FROM retcon_diff;"
    return (iks, fks, docs, diffs)

-- | Dump all tables from PostgreSQL
dumpStore
    :: PGStore
    -> IO (RetconTable, RetconFkTable, RetconInitialTable, RetconDiffs, RetconConflicts)
dumpStore (PGStore conn _) = do
    retconTable             <- query_ conn "SELECT entity, id FROM retcon ORDER BY entity, id;"
    retconFkTable           <- query_ conn "SELECT entity, id, source, fk FROM retcon_fk ORDER BY entity, id, source, fk;"
    retconInitialTable      <- query_ conn "SELECT entity, id, document FROM retcon_initial ORDER BY entity, id;"
    retconDiffTable         <- query_ conn "SELECT entity, id, diff_id, content FROM retcon_diff ORDER BY entity, id, diff_id;"
    retconDiffConflictTable <- query_ conn "SELECT operation_id, diff_id, content FROM retcon_diff_conflicts ORDER BY diff_id, operation_id;"
    return (retconTable, retconFkTable, retconInitialTable, retconDiffTable, retconDiffConflictTable)

-- | Canned query to run to check that connections are live.
onepluszero :: Connection -> IO [Only Int]
onepluszero conn = query_ conn "SELECT 1 + 0;"


--------------------------------------------------------------------------------

main :: IO ()
main = hspec postgresqlSuite

prepareDatabase :: IO () -> IO ()
prepareDatabase action = bracket setupSuite teardownSuite (const action)
  where
    db = BS.unpack dbname

    setupSuite :: IO ()
    setupSuite = do
        _ <- system $ concat [ " dropdb --if-exists ", db, " >/dev/null 2>&1 "
                             , " && createdb ", db
                             , " && psql --quiet --file=retcon.sql ", db
                             ]
        return ()

    teardownSuite :: a -> IO ()
    teardownSuite _ = do
        _ <- system $ concat [ "dropdb --if-exists ", db, " >/dev/null 2>&1 " ]
        return ()

postgresqlSuite :: Spec
postgresqlSuite = around_ prepareDatabase .
  describe "PostgreSQL storage backend" $ do
    it "should be connected when initialised" $ do
      -- postgresql-simple doesn't seem to provide a function similar to
      -- PQstatus from libpq; so let's just try *using* the connection!
      store@(PGStore conn _) <- initBackend options

      [Only one] <- onepluszero conn
      one `shouldBe` 1
      closeBackend store

    it "should be disconnected when finalised" $ do
      store@(PGStore conn _) <- initBackend options

      -- Check that the connection is now open.
      [Only one] <- onepluszero conn
      one `shouldBe` 1

      -- Check that finalising does close the connection.
      closeBackend store
      void (onepluszero conn) `shouldThrow` isDisconnected

    it "should allocate and delete internal keys" $ do
      store@(PGStore _conn _) <- initBackend options

      -- Check it's empty, so our counts will be correct.
      initial@(_iks, _fks, _docs, _diffs) <- countStore store
      initial `shouldBe` (0,0,0,0)

      initialDumps <- dumpStore store
      initialDumps `shouldBe` ([], [], [], [], [])

      -- Create some internal keys.
      keys <- runAction . liftIO $ do
          ik1 <- createInternalKey store "tests"
          ik2 <- createInternalKey store "tests"
          ik3 <- createInternalKey store "testers"

          let fk1 = ForeignKey "tests" "test" "fk1"
          let fk2 = ForeignKey "tests" "more" "fk2"
          let fk3 = ForeignKey "testers" "tester1" "fk3"

          recordForeignKey store ik1 fk1
          recordForeignKey store ik1 fk2
          recordForeignKey store ik3 fk3

          return (ik1, ik2, ik3)

      try1 <- countStore store
      try1 `shouldBe` (3, 3, 0, 0)

      try1' <- dumpStore store
      try1' `shouldBe` (
          [("testers", 3), ("tests", 1), ("tests", 2)],
          [("testers", 3, "tester1", "fk3"), ("tests", 1, "more", "fk2"), ("tests", 1, "test", "fk1")],
          [], [], [])

      -- Delete ik1 and it's associated things.
      ik1 <- case keys of
          Left  _          -> error "Couldn't create internal keys"
          Right (k1, _, _) -> return k1

      do x <- runAction . liftIO $ deleteInternalKey store ik1
         either throwIO (`shouldBe` 3) x

      try2 <- countStore store
      try2 `shouldBe` (2, 1, 0, 0)

      try2' <- dumpStore store
      try2' `shouldBe` ([("testers", 3), ("tests", 2)], [("testers", 3, "tester1", "fk3")], [], [], [])

      closeBackend store

    it "should associate foreign and internal keys" $ do
      store@(PGStore _conn _) <- initBackend options

      let fk1 = ForeignKey "tests" "test" "test1"
          fk2 = ForeignKey "tests" "test" "test2"
          fk3 = ForeignKey "tests" "test" "test3"
          fk4 = ForeignKey "tests" "more" "more1"
          fk5 = ForeignKey "tests" "more" "more2"
          fk6 = ForeignKey "tests" "more" "more3"

      ik1 <- runAction . liftIO $ do
          ik1 <- createInternalKey store "tests"
          ik2 <- createInternalKey store "tests"
          ik3 <- createInternalKey store "tests"

          recordForeignKey store ik1 fk3
          recordForeignKey store ik1 fk4
          recordForeignKey store ik2 fk2
          recordForeignKey store ik2 fk5
          recordForeignKey store ik3 fk1
          recordForeignKey store ik3 fk6

          return ik1

      -- Check there are as many things in the database as we expect.
      counts <- countStore store
      counts `shouldBe` (3, 6, 0, 0)

      contents1 <- dumpStore store
      contents1 `shouldBe` (
          [("tests", 1), ("tests", 2), ("tests", 3)],
          [("tests", 1, "more", "more1"), ("tests", 1, "test", "test3"), ("tests", 2, "more", "more2"),
          ("tests", 2, "test", "test2"), ("tests", 3, "more", "more3"), ("tests", 3, "test", "test1")],
          [], [], [])

      _ <- fmap (either throwIO return)
        . runAction . liftIO $ deleteForeignKey store fk5

      -- Check there are as many things in the database as we expect.
      counts' <- countStore store
      counts' `shouldBe` (3, 5, 0, 0)

      contents2 <- dumpStore store
      contents2 `shouldBe` (
          [("tests", 1), ("tests", 2), ("tests", 3)],
          [("tests", 1, "more", "more1"), ("tests", 1, "test", "test3"),
          ("tests", 2, "test", "test2"), ("tests", 3, "more", "more3"), ("tests", 3, "test", "test1")],
          [], [], [])

      result <- runAction . liftIO $
          case ik1 of
              Left  _ -> error "Was not able to create ik1."
              Right k -> deleteInternalKey store k

      -- Check there are as many things in the database as we expect.
      either throwIO (`shouldBe` 3) result
      counts'' <- countStore store
      counts'' `shouldBe` (2, 3, 0, 0)

      contents3 <- dumpStore store
      contents3 `shouldBe` (
          [("tests", 2), ("tests", 3)],
          [("tests", 2, "test", "test2"), ("tests", 3, "more", "more3"), ("tests", 3, "test", "test1")],
          [], [], [])

      closeBackend store

    it "should record initial documents" $ do
      store@(PGStore _conn _) <- initBackend options

      let f = Array . V.singleton

      let doc1 = Document "" "" $ f "Document One"
          doc2 = Document "" "" $ f "Document Two"
          doc3 = Document "" "" $ f "Document Three"
          doc4 = Document "" "" $ f "Document Four"

      Right (ik1, ik2, ik3, _ik4) <- runAction . liftIO $ do
          ik1 <- createInternalKey store "tests"
          ik2 <- createInternalKey store "testers"
          ik3 <- createInternalKey store "tests"
          ik4 <- createInternalKey store "tests"
          return (ik1, ik2, ik3, ik4)

      _ <- fmap (either throwIO return) . runAction . liftIO $ do
          recordInitialDocument store ik1 doc1
          recordInitialDocument store ik2 doc2
          recordInitialDocument store ik3 doc3

      -- Check it.
      count <- countStore store
      count `shouldBe` (4, 0, 3, 0)

      contents <- dumpStore store
      contents `shouldBe` (
          [("testers", 2), ("tests", 1), ("tests", 3), ("tests", 4)],
          [],
          [("testers", 2, toJSON doc2), ("tests", 1, toJSON doc1),
          ("tests", 3, toJSON doc3)],
          [], [])

      _ <- fmap (either throwIO return) . runAction . liftIO
        $ recordInitialDocument store ik3 doc4

      -- Check it.
      count' <- countStore store
      count' `shouldBe` (4, 0, 3, 0)

      contents' <- dumpStore store
      contents' `shouldBe` (
          [("testers", 2), ("tests", 1), ("tests", 3), ("tests", 4)],
          [],
          [("testers", 2, toJSON doc2), ("tests", 1, toJSON doc1),
          ("tests", 3, toJSON doc4)],
          [], [])

      _ <- fmap (either throwIO return)
        . runAction . liftIO $ deleteInitialDocument store ik2

      -- Check it.
      count'' <- countStore store
      count'' `shouldBe` (4, 0, 2, 0)

      contents'' <- dumpStore store
      contents'' `shouldBe` (
          [("testers", 2), ("tests", 1), ("tests", 3), ("tests", 4)],
          [],
          [("tests", 1, toJSON doc1), ("tests", 3, toJSON doc4)],
          [], [])

      closeBackend store

    it "should record diffs" $ do
      store@(PGStore _conn _) <- initBackend options

      -- TODO Put some actual diffs in here.
      let a1  = mempty
          l1  = []
          ds1 = (Patch Unamed a1, l1)
          a2  = mempty
          l2  = []
          ds2 = (Patch Unamed a2, l2)
          a3  = mempty
          l3  = []
          ds3 = (Patch Unamed a3, l3)

      -- Insert some initial documents.
      Right (ik1, ik2, ik3, _ik4) <- runAction . liftIO $ do
        ik1 <- createInternalKey store "tests"
        ik2 <- createInternalKey store "testers"
        ik3 <- createInternalKey store "tests"
        ik4 <- createInternalKey store "tests"
        return (ik1, ik2, ik3, ik4)

      _ <- fmap (either throwIO (`shouldBe` (1,2,3))) . runAction . liftIO $
          (,,) <$> recordDiffs store ik1 ds1
               <*> recordDiffs store ik2 ds2
               <*> recordDiffs store ik3 ds3

      count <- countStore store
      count `shouldBe` (4, 0, 0, 3)

      -- Extract operations from "conflict" diffs.
      -- TODO extract the operations from the diffs defined above.
      let expectedL1 = []
      let expectedL2 = []
      let expectedL3 = []
      let conflicts = concat [expectedL1, expectedL2, expectedL3]

      contents <- dumpStore store
      contents `shouldBe` (
          [("testers", 2), ("tests", 1), ("tests", 3), ("tests", 4)],
          [], [],
          [("testers", 2, 2, toJSON a1), ("tests", 1, 1, toJSON a2), ("tests", 3, 3, toJSON a3)],
          conflicts
          )

      -- deletes a successful and unsuccessful diff
      _ <- fmap (either throwIO (`shouldBe` 1))
        . runAction . liftIO $ deleteDiffsWithKey store ik2

      count' <- countStore store
      count' `shouldBe` (4, 0, 0, 2)

      contents' <- dumpStore store
      contents' `shouldBe` (
          [("testers", 2), ("tests", 1), ("tests", 3), ("tests", 4)],
          [], [],
          [("tests", 1, 1, toJSON a2), ("tests", 3, 3, toJSON a3)],
          expectedL1 <> expectedL3
          )

      closeBackend store

    it "should maintain a work queue" $ do
        store@PGStore{} <- initBackend options

        let work1 = WorkNotify $ ForeignKey "tests" "foo" "1"
            work2 = WorkNotify $ ForeignKey "tests" "foo" "2"
            work3 = WorkApplyPatch 3 mempty

        -- Add some WorkItems
        addWork store work1
        addWork store work2

        -- FIFO not LIFO
        Just work1' <- getWork store
        snd work1' `shouldBe` work1
        ungetWork store $ fst work1'

        -- Work we didn't complete should still be there.
        Just work1'' <- getWork store
        snd work1'' `shouldBe` work1

        -- When we do complete some work, it's not in the queue any more.
        completeWork store $ fst work1''
        work2' <- getWork store
        snd <$> work2' `shouldBe` Just work2

        let Just wid2 = fst <$> work2'
        completeWork store wid2
        nowork <- getWork store
        nowork `shouldBe` Nothing

        addWork store work3
        work3' <- getWork store
        snd <$> work3' `shouldBe` Just work3

