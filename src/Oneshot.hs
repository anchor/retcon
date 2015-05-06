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
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Version
import           Options.Applicative
import           System.Exit
import           System.FilePath

import qualified Paths_retcon               as Paths
import           Retcon                     hiding (Parser)
import           Retcon.Monad
import           Retcon.Program.Once

-- | Command line options for the server.
data Options = Options
    { optConfiguration :: FilePath
    , optCommand       :: Request
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
    <*> requestParser

-- | Parse a 'Request' from command line options.
--
-- The syntax is <command> <entity> <source> <fk>
--
-- read customer accounts 23
requestParser :: Parser Request
requestParser = subparser
    (  command "create" (info cP (progDesc "Execute creation command"))
    <> command "read"   (info rP (progDesc "Execute read command"))
    <> command "update" (info uP (progDesc "Execute update command"))
    <> command "delete" (info dP (progDesc "Execute delete command"))
    )
  where
    cP = Create <$> fkP
    rP = Read <$> fkP
    uP = Update <$> fkP
    dP = Delete <$> fkP

    fkP :: Parser ForeignKey
    fkP = ForeignKey
        <$> argument (EntityName . T.pack <$> str) (metavar "ENTITY")
        <*> argument (SourceName . T.pack <$> str) (metavar "SOURCE")
        <*> argument (T.pack <$> str) (metavar "KEY")

-- | Initialise the runtime 'Configuration' based on command line 'Options'.
withConfiguration
    :: (Request -> Configuration -> IO a)
    -> Options
    -> IO a
withConfiguration fn opt =
    bracket (configure opt) unconfigure (fn $ optCommand opt)
  where
    configure Options{..} = do
        cfg <- load [Required optConfiguration] >>= (runExceptT . parseConfiguration)

        case cfg of
            Left e -> T.putStrLn ("Could not load configuration: " <> e) >>
                      exitFailure
            Right c -> return c
    unconfigure _ = return ()

-- | Run the retcond process.
run :: Request -> Configuration -> IO ()
run req conf
  =   either (error . show) id
  <$> runRetconMonad
      (RetconEnv conf)
      (retconOnce req conf)

main :: IO ()
main = do
    etc <- Paths.getSysconfDir
    execParser (opts etc) >>= withConfiguration run
  where
    opts etc = info (helper <*> optionsParser etc)
        (  fullDesc
        <> progDesc "Retcon JSON data between multiple data sources."
        <> header ("retcon " <> showVersion version <>
                " - run retcond actions")
        )
