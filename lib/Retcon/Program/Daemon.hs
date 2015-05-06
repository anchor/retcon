--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Run /Retcon/ as a server.
module Retcon.Program.Daemon where

import           System.Log.Logger

import           Retcon.Configuration
import           Retcon.Network.Server

-- | Start the retcon daemon.
retcon
    :: Configuration
    -> IO ()
retcon cfg = do
    let (_, pri, _) = configServer cfg
    updateGlobalLogger rootLoggerName (setLevel pri)
    spawnServer cfg 1
