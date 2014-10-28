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
{-# LANGUAGE TypeFamilies          #-}

-- | Test the API round-trip.
module Main where

import Control.Concurrent.Async
import Data.ByteString ()
import Data.Proxy
import GHC.TypeLits ()
import Test.Hspec

import Retcon.Core
import Retcon.Network.Client
import Retcon.Network.Server
import Retcon.Options
import Retcon.Store.PostgreSQL

suite :: String -> Spec
suite conn = describe "Retcon API" $ do
    it "replies to conflict list requests" $ do
        result <- runRetconZMQ conn getConflicted
        result `shouldBe` Right []

    it "replies to resolve conflict requests" $ do
        let diff_id = DiffID 1
        let ops = []
        result <- runRetconZMQ conn $ enqueueResolveDiff diff_id ops
        result `shouldBe` Right ()

    it "replies to 'bad entity' notify requests with error" $ do
        let note = ChangeNotification "BadEntity" "BadSource" "item1"
        result <- runRetconZMQ conn $ enqueueChangeNotification note
        result `shouldBe` Left UnknownKeyError

    it "replies to 'bad source' notify requests with error" $ do
        let note = ChangeNotification "TestEntity" "BadSource" "item1"
        result <- runRetconZMQ conn $ enqueueChangeNotification note
        result `shouldBe` Left UnknownKeyError

    it "replies to good notify requests with success" $ do
        let note = ChangeNotification "TestEntity" "TestSource" "item1"
        result <- runRetconZMQ conn $ enqueueChangeNotification note
        result `shouldBe` Right ()

    it "replies to flush work requests with success" $ do
        result <- runRetconZMQ conn $ flushWorkQueue
        result `shouldBe` Right 0

    it "replies to invalid requests" $
        -- Right result <- runRetconZMQ conn $ performRequest InvalidHeader
        -- result `shouldbe` InvalidResponse
        pendingWith "This cannot be implemented without changing the interface."

main :: IO ()
main = do
    let conn = "tcp://127.0.0.1:1234"
    let db = "dbname=retcon_test"
    let entities = [SomeEntity (Proxy :: Proxy "TestEntity")]

    -- Prepare the retcon and server configurations.
    let serverConfig = ServerConfig conn
    let retconOpt = RetconOptions False LogStderr db Nothing
    retconConfig <- prepareConfig (retconOpt, []) entities

    -- Spawn the server.
    server <- async $ apiServer retconConfig serverConfig

    -- Run the test suite.
    hspec (suite conn)

    -- Shut the server down.
    cancel server

-- * Retcon entity

instance RetconEntity "TestEntity" where
    entitySources _ = [ SomeDataSource (Proxy :: Proxy "TestSource") ]

instance RetconDataSource "TestEntity" "TestSource" where
    data DataSourceState "TestEntity" "TestSource" = TestSourceState

    initialiseState = return TestSourceState

    finaliseState _ = return ()

    setDocument document fk = return undefined

    getDocument fk = return undefined

    deleteDocument fk = return ()
