{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Applicative
import           Data.Monoid
import qualified Data.Text                 as T
import           Options.Applicative       hiding (command)
import qualified Options.Applicative       as O
import           Options.Applicative.Types
import           System.Exit

import           Retcon.Identifier
import           Retcon.Network.Client
import           Retcon.Network.Protocol

data Options = Options
    { connection :: String
    , command    :: Command
    }

data Command
    = Notify
        { entity :: EntityName
        , source :: SourceName
        , key    :: ForeignID
        }

optionsParser :: Parser Options
optionsParser = Options
    <$> strOption (long "connection" <> help "Server ZMQ connection string")
    <*> subparser (
        O.command "notify" (info pNotify
            (progDesc "Notify retcon of a change to be processed."))
        )
  where
    pNotify = Notify
        <$> argument auto (metavar "ENTITY")
        <*> argument auto (metavar "SOURCE")
        <*> argument f    (metavar "FK")
    f = readerAsk >>= return . T.pack

run :: Options -> IO ()
run Options{..} = do
    val <- runRetconZMQ connection $ case command of
        -- Try to send a "something changed" notification.
        Notify{..} -> enqueueChangeNotification $
            ChangeNotification entity source key
    case val of
        Left  e -> do
            print e
            exitFailure
        Right _ ->
            exitSuccess

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> optionsParser)
      ( fullDesc
     <> progDesc "Interact with a retcon server."
     <> header "retcon - interact with a retcon server" )
