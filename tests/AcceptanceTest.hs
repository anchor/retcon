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

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Test the API round-trip.
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Data.ByteString ()
import Data.Proxy
import GHC.Exts (IsList (..))
import System.Directory
import System.FilePath.Posix
import Test.Hspec

import DBHelpers
import Retcon.Core
import Retcon.DataSource.JsonDirectory
import Retcon.DataSource.PostgreSQL
import Retcon.Document
import Retcon.Monad
import Retcon.Network.Client
import Retcon.Network.Server
import Retcon.Options
import Retcon.Store.PostgreSQL
import TestHelpers

instance RetconEntity "acceptance-user" where
    entitySources _ = [
        SomeDataSource (Proxy :: Proxy "local"),
        SomeDataSource (Proxy :: Proxy "upstream")
        ]

-- | Data sources are just JSON blobs in directories.

instance RetconDataSource "acceptance-user" "upstream" where
    data DataSourceState "acceptance-user" "upstream" = UpstreamUser FilePath

    initialiseState = UpstreamUser <$> testJSONFilePath
    finaliseState _ = return ()

    getDocument key =
        getActionState >>= \(UpstreamUser fp) -> getJSONDir fp key
    setDocument doc key =
        getActionState >>= \(UpstreamUser fp) -> setJSONDir fp doc key
    deleteDocument key =
        getActionState >>= \(UpstreamUser fp) -> deleteJSONDir fp key

instance RetconDataSource "acceptance-user" "local" where
    data DataSourceState "acceptance-user" "local" = LocalUser FilePath

    initialiseState = LocalUser <$> testJSONFilePath
    finaliseState _ = return ()

    getDocument key =
        getActionState >>= \(LocalUser fp) -> getJSONDir fp key
    setDocument doc key =
        getActionState >>= \(LocalUser fp) -> setJSONDir fp doc key
    deleteDocument key =
        getActionState >>= \(LocalUser fp) -> deleteJSONDir fp key

suite :: String -> Spec
suite conn =
    describe "Retcon" $ do
        -- | A modification is made upstream, Retcon is notified and it makes
        -- the appropriate change locally
        it "upstream change propogates locally" . withTestState conn $ \fk -> do
            pending

        -- | A record is removed upstream, retcon is notified, identifies this
        -- as a a delete and removes the appropriate record locally.
        it "upstream delete propogates locally" pending

        -- | A record is modified both upstream and downstream in an
        -- incompatible way. Retcon is notified of the upstream and downstream
        -- change (two notifications in total) and correctly indicates a conflict
        -- once.
        --
        -- The local change is chosen and this preference is propogated
        -- upstream.
        it "comparing and resolving diff works" pending

withTestState
    :: String
    -> (ForeignKey "acceptance-user" "upstream" -> IO a)
    -> IO a
withTestState conn f = bracket setup teardown (f . fst)
  where
    hubert :: Document
    hubert =
        [ (["name"], "Hubert")
        , (["address"], "123 Pony Avenue")
        ] ^. to fromList . from _Wrapped

    setup = do
        let db = DBName "retcon_test"
        let entities = []

        -- Prepare the retcon and server configurations.
        let server_cfg = ServerConfig conn
        let opts = RetconOptions False LogStderr (pgConnStr db) Nothing

        -- Spawn the server, giving it an initial database so that it's happy.
        resetTestDBWithFixture db "retcon.sql"
        retcon_cfg <- prepareConfig (opts, []) entities
        server <- async $ apiServer retcon_cfg server_cfg

        -- Create JSON fixture
        fp <- testJSONFilePath
        local_fk <- setJSONDir fp hubert Nothing
        let (entity,source,key) = foreignKeyValue local_fk

        -- Signal retcon to propogate this upstream
        (runRetconZMQ conn $ enqueueChangeNotification $
            ChangeNotification entity source key)
            >>= either throwIO return

        -- TODO: Don't wait here, check retcon somehow.
        threadDelay 100000
        return (local_fk, server)


    teardown (_, server) = do
        cancel server
        -- Clear all the JSON blobs out
        (</> "acceptance-user") <$> testJSONFilePath >>= removeFile

main :: IO ()
main = do
    let conn = "tcp://127.0.0.1:1234"
    -- Run the test suite.
    hspec (suite conn)
