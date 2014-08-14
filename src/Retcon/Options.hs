--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Options for running retcon from CLI and config files.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Retcon.Options where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative hiding (Parser, option)
import qualified Options.Applicative as O
import System.Directory
import Text.Trifecta

-- | Logging destinations.
data Logging =
      LogStderr
    | LogStdout
    | LogNone
  deriving (Eq, Read)

instance Show Logging where
    show LogStderr = "stderr"
    show LogStdout = "stdout"
    show LogNone   = "none"

-- | Options to control the operation of retcon.
data RetconOptions =
    RetconOptions {
          optVerbose :: Bool
        , optLogging :: Logging
        , optDB      :: ByteString
        , optArgs    :: [Text]
    }
  deriving (Show, Eq)

-- | Default options which probably won't let you do much of anything.
defaultOptions :: RetconOptions
defaultOptions = RetconOptions False LogNone "" []

-- * Configuration

-- | Parse options from a config file and/or the command line.
parseArgsWithConfig :: FilePath -> IO RetconOptions
parseArgsWithConfig = parseFile >=> execParser . helpfulParser

-- * Options parsers

-- | Parse options from the command line.
helpfulParser :: RetconOptions -> ParserInfo RetconOptions
helpfulParser os = info (helper <*> optionsParser os) fullDesc

-- | Applicative parser for options.
optionsParser :: RetconOptions -> O.Parser RetconOptions
optionsParser RetconOptions{..} =
    RetconOptions <$> parseVerbose
                  <*> parseLogging
                  <*> parseDB
                  <*> parseArgs
  where
    parseVerbose = switch $
           long "verbose"
        <> short 'v'
        <> help "Produce verbose output"
    parseDB = nullOption $
           long "db"
        <> short 'd'
        <> metavar "DATABASE"
        <> value optDB
        <> showDefault
        <> help "PostgreSQL connection string"
        <> reader (return . BS.pack)
    parseLogging = nullOption $
           long "log"
        <> short 'l'
        <> metavar "stderr|stdout|none"
        <> help "Log messages to an output"
        <> value optLogging
        <> showDefault
        <> reader readLog
    parseArgs = many $ argument (return . T.pack) $
           metavar "FILES..."

-- | Reader for logging options.
readLog :: (Monad m, MonadPlus m) => String -> m Logging
readLog "stderr" = return LogStderr
readLog "stdout" = return LogStdout
readLog "none"   = return LogNone
readLog _        = mzero

-- * Config file parsers

-- | Parse options from a config file.
parseFile :: FilePath -> IO RetconOptions
parseFile path = do
    exists <- doesFileExist path
    if exists
    then do
        maybe_ls <- parseFromFile configParser path
        case maybe_ls of
            Just ls -> return $ mergeConfig ls defaultOptions
            Nothing -> return defaultOptions
    else return defaultOptions
  where
    mergeConfig ls RetconOptions{..} = fromJust $
        RetconOptions <$> pure optVerbose
                      <*> (lookup "logging" ls >>= readLog) `mplus` pure optLogging
                      <*> liftM BS.pack (lookup "database" ls) `mplus` pure optDB
                      <*> pure []

    configParser :: Parser [(String, String)]
    configParser = some $ liftA2 (,)
        (spaces *> possibleKeys <* spaces <* char '=')
        (spaces *> (stringLiteral <|> stringLiteral'))

    possibleKeys :: Parser String
    possibleKeys = string "logging" <|> string "database"
