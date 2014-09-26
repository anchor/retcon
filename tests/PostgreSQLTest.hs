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

module Main where

import Control.Monad.IO.Class
import Data.ByteString
import Data.Proxy
import GHC.TypeLits
import System.Process
import Test.Hspec

import Retcon.Config
import Retcon.DataSource
import Retcon.DataSource.PostgreSQL
import Retcon.Handler
import Retcon.Monad

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
            {-
            state <- initialiseState
            _ <- runDataSourceAction state $
                getDocument (ForeignKey "1" :: ForeignKey "customer" "db1")
            finaliseState state
            -}
            pendingWith "Refactor after DataSourceAction monad change"

        it "can load row 2 from db1" $ do
            {-
            state <- initialiseState
            _ <- runDataSourceAction state $
                getDocument (ForeignKey "2" :: ForeignKey "customer" "db1")
            finaliseState state
            -}
            pendingWith "Refactor after DataSourceAction monad change"

        it "can write db1/row1 to db2 with new key" $ do
            {-
            state <- initialiseState
            Right doc3 <- runDataSourceAction state $
                getDocument (ForeignKey "1" :: ForeignKey "customer" "db1")
            finaliseState state

            state2 <- initialiseState
            _ <- runDataSourceAction state2 $
                setDocument doc3 (Nothing :: Maybe (ForeignKey "customer" "db2"))
            finaliseState state2
            -}
            pendingWith "Refactor after DataSourceAction monad change"

        it "can write db1/row2 to db2 with existing key row1" $ do
            {-
            state <- initialiseState
            Right doc4 <- runDataSourceAction state $
                getDocument (ForeignKey "2" :: ForeignKey "customer" "db1")
            finaliseState state
            state2 <- initialiseState
            _ <- runDataSourceAction state2 $
                setDocument doc4 (Just $ ForeignKey "1" :: Maybe (ForeignKey "customer" "db2"))
            finaliseState state2
            -}
            pendingWith "Refactor after DataSourceAction monad change"

        it "can delete db2/row1" $ do
            {-
            state <- initialiseState
            res <- runDataSourceAction state $ do
                doc5 <- getDocument (ForeignKey "1" :: ForeignKey "customer" "db2")
                deleteDocument (ForeignKey "1" :: ForeignKey "customer" "db2")
            finaliseState state
            -}
            pendingWith "Refactor after DataSourceAction monad change"

-- | get source file path
sourceDb :: ByteString
sourceDb = "dbname='retcon_pg_test'"

-- | get target file path
targetDb :: ByteString
targetDb = "dbname='retcon_pg_test2'"

-- | Explicitly pass a test.
pass :: Expectation
pass = return ()

main :: IO ()
main = do
    ex1 <- system "dropdb --if-exists retcon_pg_test && createdb retcon_pg_test && psql retcon_pg_test -f tests/data/retcon_pg_test.sql"
    ex2 <- system "dropdb --if-exists retcon_pg_test2 && createdb retcon_pg_test2 && psql retcon_pg_test2 -f tests/data/retcon_pg_test2.sql"
    hspec suite
    where
        cfg = RetconConfig [ SomeEntity (Proxy :: Proxy "customer") ]

