--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

module DBHelpers where

import Data.List
import Data.Monoid
import Retcon.DataSource.PostgreSQL
import System.Exit
import System.FilePath.Posix
import System.Process

-- This is not a safe function and could be exploited for command execution.
resetTestDBWithFixture :: DBName -> FilePath -> IO ()
resetTestDBWithFixture (DBName db) fixture = do
    x <- system . intercalate " && " $
        [ "dropdb --if-exists " <> db
        , "createdb " <> db
        , "psql " <> db <> " -f " <> fixture
        ]
    case x of
        ExitSuccess ->
            return ()
        ExitFailure n ->
            error $ "Failed to reset test db, cmd failed with " ++ show n
