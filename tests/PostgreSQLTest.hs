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
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Exception
import Control.Lens.Operators
import Control.Monad.IO.Class
import Data.Monoid
import Data.Proxy
import DBHelpers
import System.FilePath.Posix
import Test.Hspec

import Retcon.Core
import Retcon.DataSource.PostgreSQL
import Retcon.Error
import Retcon.Monad
import Retcon.Options
import Retcon.Store.Memory

instance RetconEntity "customer" where
    entitySources _ = [
        SomeDataSource (Proxy :: Proxy "db1"),
        SomeDataSource (Proxy :: Proxy "db2")
        ]

instance RetconDataSource "customer" "db1" where

    data DataSourceState "customer" "db1" = CustDB1

    initialiseState = return CustDB1

    finaliseState CustDB1 = return ()

    getDocument key = liftIO $ do
        res <- getPgDocument sourceDb key
        either (error . show) return res

    setDocument doc key = liftIO $ do
        res <- setPgDocument sourceDb doc key
        either (error . show) (\x ->
            case x of
                Nothing -> error "No key"
                Just y  -> return y) res

    deleteDocument key = liftIO $ do
        res <- deletePgDocument sourceDb key
        either (error . show) return res

instance RetconDataSource "customer" "db2" where

    data DataSourceState "customer" "db2" = CustDB2

    initialiseState = return CustDB2

    finaliseState CustDB2 = return ()


    getDocument key = liftIO $ do
        res <- getPgDocument targetDb key
        either (error . show) return res

    setDocument doc key = liftIO $ do
        res <- setPgDocument targetDb doc key
        either (error . show) (\x ->
            case x of
                Nothing -> error "No key"
                Just y  -> return y) res

    deleteDocument key = liftIO $ do
        res <- deletePgDocument targetDb key
        either (error . show) return res

suite :: Spec
suite =
    describe "PostgreSQL marshalling" $ do
        it "can load row 1 from db1" $ do
            state <- runInitialiser mempty initialiseState
            _ <- run state $
                getDocument (ForeignKey "1" :: ForeignKey "customer" "db1")
            runInitialiser mempty $ finaliseState state
            pass

        it "can load row 2 from db1" $ do
            state <- runInitialiser mempty initialiseState
            _ <- run state $
                getDocument (ForeignKey "2" :: ForeignKey "customer" "db1")
            runInitialiser mempty $ finaliseState state
            pass

        it "can write db1/row1 to db2 with new key" $ do
            state <- runInitialiser mempty initialiseState
            Right doc3 <- run state $
                getDocument (ForeignKey "1" :: ForeignKey "customer" "db1")
            runInitialiser mempty $ finaliseState state

            state2 <- runInitialiser mempty initialiseState
            _ <- run state2 $
                setDocument doc3 (Nothing :: Maybe (ForeignKey "customer" "db2"))
            runInitialiser mempty $ finaliseState state2
            pass

        it "can write db1/row2 to db2 with existing key row1" $ do
            let fk1 = ForeignKey "2" :: ForeignKey "customer" "db1"

            state <- runInitialiser mempty initialiseState
            Right doc4 <- run state $
                getDocument fk1
            runInitialiser mempty $ finaliseState state

            let fk2 = ForeignKey "1" :: ForeignKey "customer" "db2"
            state2 <- runInitialiser mempty initialiseState
            Right res2 <- run state2 $
                setDocument doc4 (Just fk2)
            runInitialiser mempty $ finaliseState state2

            res2 `shouldBe` fk2

        it "can delete db2/row1" $ do
            let fk2 = ForeignKey "1" :: ForeignKey "customer" "db2"

            state <- runInitialiser mempty initialiseState
            res1 <- run state $
                getDocument fk2

            case res1 of
                Left  _ -> error "Couldn't get the document."
                Right _ -> return ()

            res2 <- run state $
                deleteDocument fk2
            runInitialiser mempty $ finaliseState state
            either throwIO return res2

sourceDb :: DBName
sourceDb = "retcon_pg_test"

targetDb :: DBName
targetDb = "retcon_pg_test2"

-- | Explicitly pass a test.
pass :: Expectation
pass = return ()

-- | Helper to execute an "action".
run :: l -> RetconMonad InitialisedEntity ROToken l r -> IO (Either RetconError r)
run l a = do
    store <- storeInitialise opt :: IO MemStorage
    let store' = restrictToken . token $ store
    let cfg = RetconConfig
                (opt ^. optVerbose)
                (opt ^. optLogging)
                store'
                mempty
                []
                state
    result <- runRetconMonad (RetconMonadState cfg l) a
    storeFinalise store
    return result
  where
    opt = defaultOptions
    state = []

fixturePath :: FilePath
fixturePath = "tests" </> "data" </> "sql"

main :: IO ()
main = do
    resetTestDBWithFixture sourceDb $ fixturePath </> "retcon_pg_test.sql"
    resetTestDBWithFixture targetDb $ fixturePath </> "retcon_pg_test2.sql"
    hspec suite
