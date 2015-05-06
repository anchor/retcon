--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.Trans.Except
import           Data.Configurator
import           Data.Monoid
import qualified Data.Text.IO               as T
import           Data.Version
import           Options.Applicative
import           System.Exit
import           System.FilePath

import qualified Paths_retcon               as Paths
import           Retcon                     hiding (Parser)
import           Retcon.Program.Daemon

-- | Command line options for the server.
data Options = Options
    { optConfiguration :: FilePath
    }
  deriving (Show, Eq)

optionsParser :: FilePath -> Parser Options
optionsParser etc = Options
    <$> option str
        (  long "config"
        <> short 'c'
        <> help "Configuration file"
        <> metavar "FILE"
        <> value (etc </> "retcond" </> "retcond.conf")
        <> showDefault
        )

-- | Initialise the runtime 'Configuration' based on command line 'Options'.
withConfiguration
    :: (Configuration -> IO a)
    -> Options
    -> IO a
withConfiguration fn opt =
    bracket (configure opt) unconfigure fn
  where
    configure Options{..} = do
        cfg <- load [Required optConfiguration] >>= (runExceptT . parseConfiguration)

        case cfg of
            Left e -> T.putStrLn ("Could not load configuration: " <> e) >>
                      exitFailure
            Right c -> return c
    unconfigure _ = return ()

main :: IO ()
main = do
    etc <- Paths.getSysconfDir
    execParser (opts etc) >>= withConfiguration retcon
  where
    opts etc = info (helper <*> optionsParser etc)
        (  fullDesc
        <> progDesc "Retcon JSON data between multiple data sources."
        <> header ("retcond " <> showVersion version <>
                " - data synchronisation daemon")
        )
