{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- | Description: Define and operate on data sources.
--
-- /Retcon/ interacts with external systems and propagates changes to
-- duplicated data between them; these external systems are 'DataSource's.
--
module Retcon.DataSource
  ( -- * Definitions
    DataSource(..)
  , Command
  , DataSourceError(..)
  , DSMonad
  , runDSMonad

    -- * Operations
  , createDocument
  , readDocument
  , updateDocument
  , deleteDocument

  ) where

import           Control.Applicative
import           Control.Exception          (Exception)
import           Control.Monad
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Aeson
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Char
import           Data.List
import           Data.Monoid
import           Data.String                ()
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Typeable              (Typeable)
import           System.Exit
import           System.IO
import           System.Log.Logger
import           System.Process
import           Text.Regex

import           Retcon.Configuration
import           Retcon.Document
import           Retcon.Identifier


logName :: String
logName = "Retcon.DataSource"

type ErrorMsg = String

-- | Errors which can occur when invoking a data source command.
data DataSourceError
  = DecodeError            ErrorMsg -- ^ Command returned invalid JSON.
  | CommunicationError     Text     -- ^ Could not communicate with external system.
  | NotFoundError          Text     -- ^ External system couldn't find the document.
  | ForeignError           Int Text
  | IncompatibleDataSource ErrorMsg
  deriving (Eq, Show, Typeable)

instance Exception DataSourceError

newtype DSMonad m a = DSMonad { unDSMonad :: ExceptT DataSourceError m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadError DataSourceError)

runDSMonad :: DSMonad m a -> m (Either DataSourceError a)
runDSMonad = runExceptT . unDSMonad

-- | Replace a named hole in a string.
subNamedHole
    :: String    -- ^ hole name
    -> String    -- ^ Input string
    -> String    -- ^ Replacement text
    -> String    -- ^ Output string
subNamedHole name = subRegex . mkRegex $ name

-- | Prepare a 'Command' by interpolating
prepareCommand
    :: DataSource
    -> Maybe ForeignKey
    -> Command
    -> String
prepareCommand _ds fk cmd =
    case fk of
        Nothing -> T.unpack . unCommand $ cmd
        Just ForeignKey{..} ->
            let cmd' = T.unpack . unCommand $ cmd
            in subNamedHole "%fk" cmd' (T.unpack fkID)

-- | Check that a 'DataSource' and a 'ForeignKey' are compatible, otherwise
-- raise an error in the monad.
checkCompatibility
    :: ( Synchronisable a, Synchronisable b
       , Show a, Show b
       , MonadIO m)
    => a
    -> b
    -> DSMonad m ()
checkCompatibility a b
  = unless (compatibleSource a b)
  $ throwError (IncompatibleDataSource $ show a <> " is not compatible with " <> show b)

-- | Access a 'DataSource' and create a new 'Document' returning the
-- 'ForeignKey' which, in that source, identifies the document.
--
-- It is an error if the 'DataSource' and 'Document' supplied do not agree on
-- the entity and source names.
createDocument
    :: (MonadIO m, Functor m)
    => DataSource
    -> Document
    -> DSMonad m ForeignKey
createDocument src doc = do
    -- 1. Check source and key are compatible.
    checkCompatibility src doc

    -- 2. Spawn process.
    let cmd = prepareCommand src Nothing . commandCreate $ src
        process = (shell cmd) { std_out = CreatePipe
                              , std_in = CreatePipe
                              , std_err = CreatePipe
                              }
    debug src $ "CREATE: " <> show cmd
    (Just hin, Just hout, Just herr, hproc) <- liftIO $ createProcess process

    -- 3. Write input.
    liftIO . BSL.hPutStrLn hin . encode . _documentContent $ doc
    liftIO $ hClose hin

    -- 4. Read handles
    (exit, output, err) <- liftIO $ gatherOutput (hproc, hout, herr)
    let output' = T.filter (not . isSpace) $ T.decodeUtf8 output
    let err' = T.decodeUtf8 err

    -- 5. Log and return the result.
    logDSMessage src "CREATE" (exit, err)
    case exit of
        ExitFailure 1 -> throwError $ CommunicationError err'
        ExitFailure c -> throwError $ ForeignError c err'
        ExitSuccess   -> return $
            ForeignKey (sourceEntity src) (sourceName src) output'

-- | Access a 'DataSource' and retrieve the 'Document' identified, in that source,
-- by the given 'ForeignKey'.
--
-- It is an error if the 'DataSource' and 'ForeignKey' supplied do not agree on
-- the entity and source names.
readDocument
    :: (MonadIO m, Functor m)
    => DataSource
    -> ForeignKey
    -> DSMonad m Document
readDocument src fk = do
    -- 1. Check source and key are compatible.
    checkCompatibility src fk

    -- 2. Spawn process.
    let cmd     = prepareCommand src (Just fk) . commandRead $ src
        process = (shell cmd) { std_out = CreatePipe
                              , std_err = CreatePipe
                              }
    debug src $ "READ: " <> show cmd
    (Nothing, Just hout, Just herr, hproc) <- liftIO $ createProcess process

    -- 3. Read output.
    (exit, output, err) <- liftIO $ gatherOutput (hproc, hout, herr)
    let err' = T.decodeUtf8 err

    -- 4. Check return code, raising error if required.
    logDSMessage src "READ" (exit, err)
    case exit of
        ExitFailure 1 -> throwError $ CommunicationError err'
        ExitFailure 2 -> throwError $ NotFoundError err'
        ExitFailure c -> throwError $ ForeignError c err'
        ExitSuccess   ->
            case eitherDecode' . BSL.fromStrict $ output of
                Left e  -> throwError $ DecodeError e
                Right j -> return $ Document (fkEntity fk) (fkSource fk) j

-- | Access a 'DataSource' and save the 'Document' under the specified
-- 'ForeignKey', returning the 'ForeignKey' to use for the updated document in
-- future.
--
-- It is an error if the 'DataSource' and 'ForeignKey' supplied do not agree on
-- the entity and source names.
updateDocument
    :: (MonadIO m, Functor m)
    => DataSource
    -> ForeignKey
    -> Document
    -> DSMonad m ForeignKey -- ^ New (or old) key for this document.
updateDocument src fk doc = do
    -- 1. Check source, key, and document are compatible.
    checkCompatibility src fk
    checkCompatibility src doc

    -- 2. Spawn process.
    let cmd     = prepareCommand src (Just fk) . commandUpdate $ src
        process = (shell cmd) { std_out = CreatePipe
                              , std_in = CreatePipe
                              , std_err = CreatePipe
                              }
    debug src $ "UPDATE: " <> show cmd
    (Just hin, Just hout, Just herr, hproc) <- liftIO $ createProcess process

    -- 3. Write input.
    liftIO . BSL.hPutStrLn hin . encode . _documentContent $ doc
    liftIO $ hClose hin

    -- 4. Read handles
    (exit, output', err) <- liftIO $ gatherOutput (hproc, hout, herr)
    let output = T.filter (not. isSpace) . T.decodeUtf8 $ output'
    let err' = T.decodeUtf8 err

    -- 5. Check return code, raising error if required.
    logDSMessage src "UPDATE" (exit, err)
    case exit of
        ExitFailure 1 -> throwError $ CommunicationError err'
        ExitFailure 2 -> throwError $ NotFoundError err'
        ExitFailure c -> throwError $ ForeignError c err'
        ExitSuccess   -> return $
            ForeignKey (fkEntity fk) (fkSource fk) output

-- | Access a 'DataSource' and delete the 'Document' identified in that source
-- by the given 'ForeignKey'.
--
-- It is an error if the 'DataSource' and 'ForeignKey' do not agree on the
-- entity and source names.
deleteDocument
    :: (MonadIO m, Functor m)
    => DataSource
    -> ForeignKey
    -> DSMonad m ()
deleteDocument src fk = do
    -- 1. Check source and key are compatible.
    checkCompatibility src fk

    -- 2. Spawn process.
    let cmd     = prepareCommand src (Just fk) . commandDelete $ src
        process = (shell cmd) { std_out = CreatePipe
                              , std_err = CreatePipe
                              }
    debug src $ "DELETE: " <> show cmd
    (Nothing, Just hout, Just herr, hproc) <- liftIO $ createProcess process

    -- 3. Read output
    (exit, _output, err) <- liftIO $ gatherOutput (hproc, hout, herr)
    let err' = T.decodeUtf8 err

    -- 4. Check return code, raising error if required.
    logDSMessage src "DELETE" (exit, err)
    case exit of
        ExitFailure 1 -> throwError $ CommunicationError err'
        ExitFailure 2 -> return () -- So we didn't need to do anything.
        ExitFailure c -> throwError $ ForeignError c err'
        ExitSuccess   -> return ()

-- * Utility

-- | Gather the stdout and stderr of a process, collecting the output in
-- buffers until the process terminates.
gatherOutput
    :: (ProcessHandle, Handle, Handle)
    -> IO (ExitCode, BS.ByteString, BS.ByteString)
gatherOutput (ph, outh, errh) = go mempty mempty
  where
    go outs errs = do
        -- Read any outstanding output.
        outsmore <- BS.hGetNonBlocking outh (64 * 1024)
        let outs' = outs <> outsmore
        -- Read any outstanding errors.
        errsmore <- BS.hGetNonBlocking errh (64 * 1024)
        let errs' = errs <> errsmore
        -- Check the exit status and either loop or return.
        status <- getProcessExitCode ph
        case status of
            Nothing -> go outs' errs'
            Just ec -> do
                -- Fish any leftovers out of the pipes.
                lastouts <- BS.hGetContents outh
                lasterrs <- BS.hGetContents errh
                return ( ec
                       , outs' <> lastouts
                       , errs' <> lasterrs
                       )

-- | Log data source error messages.
logDSMessage
    :: MonadIO m
    => DataSource
    -> String
    -> (ExitCode, BS.ByteString)
    -> m ()
logDSMessage src cmd (ec, err) =
    let msg = cmd <> ": " <> BS.unpack err
        name = intercalate "." [logName
                               , show (sourceEntity src)
                               , show (sourceName src)
                               ]
    in liftIO $ case ec of
        ExitSuccess   -> infoM  name msg
        ExitFailure 1 -> alertM name msg
        ExitFailure 2 -> errorM name msg
        ExitFailure 3 -> errorM name msg
        ExitFailure _ -> errorM name msg

-- | Log a debugging message.
debug
    :: MonadIO m
    => DataSource
    -> String
    -> m ()
debug src msg =
    let name = intercalate "." [logName
                               , show (sourceEntity src)
                               , show (sourceName src)
                               ]
    in liftIO . debugM name $ msg
