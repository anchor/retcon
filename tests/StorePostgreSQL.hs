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

import Control.Applicative
import Control.Exception
import Control.Lens.Operators
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Monoid
import Database.PostgreSQL.Simple
import System.Process
import Test.Hspec

import Retcon.Core
import Retcon.Document
import Retcon.Error
import Retcon.Monad
import Retcon.Options
import Retcon.Store.PostgreSQL

dbname :: BS.ByteString
dbname = "retcon_test"

options :: RetconOptions
options = defaultOptions & optDB .~ "dbname=" <> dbname

runAction :: PGStorage
          -> RetconMonad InitialisedEntity RWToken () r
          -> IO (Either RetconError r)
runAction store =
    let cfg = RetconConfig
                (options ^. optVerbose)
                (options ^. optLogging)
                (token store)
                mempty
                []
                []
    in runRetconMonad (RetconMonadState cfg ())

-- | Canned query to run to check that connections are live.
onepluszero :: Connection -> IO [Only Int]
onepluszero conn = query_ conn "SELECT 1 + 0;"

-- | Check that a SqlError exception is about a disconnected connection.
isDisconnected :: SqlError -> Bool
isDisconnected e = sqlErrorMsg e == "connection disconnected"

-- | Count the objects in the store.
countStore :: PGStorage -> IO (Int, Int, Int, Int)
countStore (PGStore conn _) = do
    [Only iks] <- query_ conn "SELECT count(*) FROM retcon;"
    [Only fks] <- query_ conn "SELECT count(*) FROM retcon_fk;"
    [Only docs] <- query_ conn "SELECT count(*) FROM retcon_initial;"
    [Only diffs] <- query_ conn "SELECT count(*) FROM retcon_diff;"
    return (iks, fks, docs, diffs)

-- | Bunch of data structures to track table contents
type RetconTable = [(String, Int)]
type RetconFkTable = [(String, Int, String, String)]
type RetconInitialTable = [(String, Int, Value)]
type RetconDiffs = [(String, Int, Int, Value)]
type RetconConflicts = [(Int, Int, Value)]

-- | Dump all tables from PostgreSQL
dumpStore
    :: PGStorage
    -> IO (RetconTable, RetconFkTable, RetconInitialTable, RetconDiffs, RetconConflicts)
dumpStore (PGStore conn _) = do
    retconTable             <- query_ conn "SELECT entity, id FROM retcon ORDER BY entity, id;"
    retconFkTable           <- query_ conn "SELECT entity, id, source, fk FROM retcon_fk ORDER BY entity, id, source, fk;"
    retconInitialTable      <- query_ conn "SELECT entity, id, document FROM retcon_initial ORDER BY entity, id;"
    retconDiffTable         <- query_ conn "SELECT entity, id, diff_id, content FROM retcon_diff ORDER BY entity, id, diff_id;"
    retconDiffConflictTable <- query_ conn "SELECT operation_id, diff_id, content FROM retcon_diff_conflicts ORDER BY diff_id, operation_id;"
    return (retconTable, retconFkTable, retconInitialTable, retconDiffTable, retconDiffConflictTable)

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
postgresqlSuite = around prepareDatabase $
    describe "PostgreSQL storage backend" $ do
        it "should be connected when initialised" $ do
            -- postgresql-simple doesn't seem to provide a function similar to
            -- PQstatus from libpq; so let's just try *using* the connection!
            store@(PGStore conn _) <- storeInitialise options

            [Only one] <- onepluszero conn
            one `shouldBe` 1

            storeFinalise store

        it "should be disconnected when finalised" $ do
            store@(PGStore conn _) <- storeInitialise options

            -- Check that the connection is now open.
            [Only one] <- onepluszero conn
            one `shouldBe` 1

            -- Check that finalising does close the connection.
            storeFinalise store
            --(void $ onepluszero conn) `shouldThrow` isDisconnected

        it "should allocate and delete internal keys" $ do
            store@(PGStore conn _) <- storeInitialise options

            -- Check it's empty, so our counts will be correct.
            initial@(iks, fks, docs, diffs) <- countStore store
            initial `shouldBe` (0,0,0,0)

            initialDumps <- dumpStore store
            initialDumps `shouldBe` ([], [], [], [], [])

            -- Create some internal keys.
            keys <- runAction store $ do
                (ik1 :: InternalKey "tests") <- createInternalKey
                (ik2 :: InternalKey "tests") <- createInternalKey
                (ik3 :: InternalKey "testers") <- createInternalKey

                let (fk1 :: ForeignKey "tests" "test") = ForeignKey "fk1"
                let (fk2 :: ForeignKey "tests" "more") = ForeignKey "fk2"
                let (fk3 :: ForeignKey "testers" "tester1") = ForeignKey "fk3"

                recordForeignKey ik1 fk1
                recordForeignKey ik1 fk2
                recordForeignKey ik3 fk3

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

            runAction store (deleteInternalKey ik1)
                >>= either throwIO (`shouldBe` 3)

            try2 <- countStore store
            try2 `shouldBe` (2, 1, 0, 0)

            try2' <- dumpStore store
            try2' `shouldBe` ([("testers", 3), ("tests", 2)], [("testers", 3, "tester1", "fk3")], [], [], [])

            storeFinalise store

        it "should associate foreign and internal keys" $ do
            store@(PGStore conn _) <- storeInitialise options

            let (fk1 :: ForeignKey "tests" "test") = ForeignKey "test1"
            let (fk2 :: ForeignKey "tests" "test") = ForeignKey "test2"
            let (fk3 :: ForeignKey "tests" "test") = ForeignKey "test3"
            let (fk4 :: ForeignKey "tests" "more") = ForeignKey "more1"
            let (fk5 :: ForeignKey "tests" "more") = ForeignKey "more2"
            let (fk6 :: ForeignKey "tests" "more") = ForeignKey "more3"

            ik1 <- runAction store $ do
                (ik1 :: InternalKey "tests") <- createInternalKey
                (ik2 :: InternalKey "tests") <- createInternalKey
                (ik3 :: InternalKey "tests") <- createInternalKey

                recordForeignKey ik1 fk3
                recordForeignKey ik1 fk4
                recordForeignKey ik2 fk2
                recordForeignKey ik2 fk5
                recordForeignKey ik3 fk1
                recordForeignKey ik3 fk6

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

            runAction store (deleteForeignKey fk5)
                >>= either throwIO return

            -- Check there are as many things in the database as we expect.
            counts <- countStore store
            counts `shouldBe` (3, 5, 0, 0)

            contents2 <- dumpStore store
            contents2 `shouldBe` (
                [("tests", 1), ("tests", 2), ("tests", 3)],
                [("tests", 1, "more", "more1"), ("tests", 1, "test", "test3"),
                ("tests", 2, "test", "test2"), ("tests", 3, "more", "more3"), ("tests", 3, "test", "test1")],
                [], [], [])

            result <- runAction store $
                case ik1 of
                    Left  _ -> error "Was not able to create ik1."
                    Right k -> deleteInternalKey k

            -- Check there are as many things in the database as we expect.
            either throwIO (`shouldBe` 3) result
            counts <- countStore store
            counts `shouldBe` (2, 3, 0, 0)

            contents3 <- dumpStore store
            contents3 `shouldBe` (
                [("tests", 2), ("tests", 3)],
                [("tests", 2, "test", "test2"), ("tests", 3, "more", "more3"), ("tests", 3, "test", "test1")],
                [], [], [])

            storeFinalise store

        it "should record initial documents" $ do
            store@(PGStore conn _) <- storeInitialise options

            let doc1 = fromJust $ mkNode (Just "Document One")
            let doc2 = fromJust $ mkNode (Just "Document Two")
            let doc3 = fromJust $ mkNode (Just "Document Three")
            let doc4 = fromJust $ mkNode (Just "Document Four")

            Right (ik1, ik2, ik3, ik4) <- runAction store $ do
                (ik1 :: InternalKey "tests") <- createInternalKey
                (ik2 :: InternalKey "testers") <- createInternalKey
                (ik3 :: InternalKey "tests") <- createInternalKey
                (ik4 :: InternalKey "tests") <- createInternalKey
                return (ik1, ik2, ik3, ik4)

            (runAction store $ do
                recordInitialDocument ik1 doc1
                recordInitialDocument ik2 doc2
                recordInitialDocument ik3 doc3) >>= either throwIO return

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

            runAction store (recordInitialDocument ik3 doc4)
                >>= either throwIO return

            -- Check it.
            count <- countStore store
            count `shouldBe` (4, 0, 3, 0)

            contents <- dumpStore store
            contents `shouldBe` (
                [("testers", 2), ("tests", 1), ("tests", 3), ("tests", 4)],
                [],
                [("testers", 2, toJSON doc2), ("tests", 1, toJSON doc1),
                ("tests", 3, toJSON doc4)],
                [], [])

            result <- runAction store (deleteInitialDocument ik2)
                >>= either throwIO return

            -- Check it.
            count <- countStore store
            count `shouldBe` (4, 0, 2, 0)

            contents <- dumpStore store
            contents `shouldBe` (
                [("testers", 2), ("tests", 1), ("tests", 3), ("tests", 4)],
                [],
                [("tests", 1, toJSON doc1), ("tests", 3, toJSON doc4)],
                [], [])

            storeFinalise store

        it "should record diffs" $ do
            store@(PGStore conn _) <- storeInitialise options

            -- TODO Put some actual diffs in here.
            let a1 = mempty
            let l1 = []
            let ds1 = (a1, l1)
            let a2 = mempty
            let l2 = [mempty]
            let ds2 = (a2, l2)
            let a3 = mempty
            let l3 = [mempty]
            let ds3 = (a3, l3)

            -- Insert some initial documents.
            Right (ik1, ik2, ik3, ik4) <- runAction store $ do
                (ik1 :: InternalKey "tests") <- createInternalKey
                (ik2 :: InternalKey "testers") <- createInternalKey
                (ik3 :: InternalKey "tests") <- createInternalKey
                (ik4 :: InternalKey "tests") <- createInternalKey
                return (ik1, ik2, ik3, ik4)

            result <- runAction store $
                (,,) <$> recordDiffs ik1 ds1
                     <*> recordDiffs ik2 ds2
                     <*> recordDiffs ik3 ds3

            either throwIO (`shouldBe` (1,2,3)) result
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

            result <- runAction store $
                deleteDiffs ik2

            either throwIO (`shouldBe` 1) result -- deletes a successful and unsuccessful diff
            count <- countStore store
            count `shouldBe` (4, 0, 0, 2)

            contents <- dumpStore store
            contents `shouldBe` (
                [("testers", 2), ("tests", 1), ("tests", 3), ("tests", 4)],
                [], [],
                [("tests", 1, 1, toJSON a2), ("tests", 3, 3, toJSON a3)],
                concat [expectedL1, expectedL3]
                )

            storeFinalise store

        it "should maintain a work queue" $ do
            store@PGStore{} <- storeInitialise options

            let work1 = WorkNotify ("tests", "foo", "1")
            let work2 = WorkNotify ("tests", "foo", "2")

            let work3 = WorkApplyPatch 3 mempty

            -- Add some WorkItems
            storeAddWork store work1
            storeAddWork store work2

            -- Claiming a work item should get the first one.
            work1' <- storeGetWork store
            snd <$> work1' `shouldBe` Just work1

            -- Claiming another work item should get the second one.
            work2' <- storeGetWork store
            snd <$> work2' `shouldBe` Just work2

            -- Marking as complete should not explode.
            let Just wid1 = fst <$> work1'
            storeCompleteWork store wid1

            -- Marking as complete should not explode.
            let Just wid2 = fst <$> work2'
            storeCompleteWork store wid2

            -- The empty queue should return nothing.
            nowork <- storeGetWork store
            nowork `shouldBe` Nothing

            -- Adding another item should work.
            storeAddWork store work3

            -- Getting it should work.
            work3' <- storeGetWork store
            snd <$> work3' `shouldBe` Just work3

-- $ Entities and Data Sources
--
-- This test suite uses two data entities, each with two data sources, in all
-- tests. This helps check that we're distinguishing internal and foreign keys
-- correctly, etc.

instance RetconEntity "tests" where
    entitySources _ = []

instance RetconEntity "testers" where
    entitySources _ = []

instance RetconDataSource "tests" "test" where

    data DataSourceState "tests" "test" = TestsTest
    initialiseState = return TestsTest
    finaliseState _ = return ()

    setDocument _ _ = undefined

    getDocument _ = undefined

    deleteDocument _ = return ()

instance RetconDataSource "tests" "more" where

    data DataSourceState "tests" "more" = TestsMore
    initialiseState = return TestsMore
    finaliseState _ = return ()

    setDocument _ _ = undefined

    getDocument _ = undefined

    deleteDocument _ = return ()

instance RetconDataSource "testers" "tester1" where

    data DataSourceState "testers" "tester1" = TestersTester1
    initialiseState = return TestersTester1
    finaliseState _ = return ()

    setDocument _ _ = undefined

    getDocument _ = undefined

    deleteDocument _ = return ()

instance RetconDataSource "testers" "tester2" where

    data DataSourceState "testers" "tester2" = TestersTester2
    initialiseState = return TestersTester2
    finaliseState _ = return ()

    setDocument _ _ = undefined

    getDocument _ = undefined

    deleteDocument _ = return ()
