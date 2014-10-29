--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Server component for the retcon network API.
module Retcon.Network.Server where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import Data.Binary
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as LBS
import Data.Either
import Data.List.NonEmpty hiding (filter, length, map)
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Typeable
import Options.Applicative
import System.ZMQ4.Monadic hiding (async)

import Retcon.Core
import Retcon.Diff
import Retcon.Document
import Retcon.Error
import Retcon.Handler
import Retcon.Monad
import Retcon.Options

-- | Values describing error states of the retcon API.
data RetconAPIError
    = UnknownServerError
    | TimeoutError
    | DecodeError
    | InvalidNumberOfMessageParts
    | UnknownKeyError -- ^ Notification contained an unknown key.
  deriving (Show, Eq, Typeable)

instance Exception RetconAPIError

instance Enum RetconAPIError where
    fromEnum TimeoutError = 0
    fromEnum InvalidNumberOfMessageParts = 1
    fromEnum DecodeError = 2
    fromEnum UnknownKeyError = 3
    fromEnum UnknownServerError = maxBound

    toEnum 0 = TimeoutError
    toEnum 1 = InvalidNumberOfMessageParts
    toEnum 2 = DecodeError
    toEnum 3 = UnknownKeyError
    toEnum _ = UnknownServerError

-- | An opaque reference to a Diff, used to uniquely reference the conflicted
-- diff for resolveDiff.
newtype DiffID = DiffID
    { unDiffID :: Int }
  deriving (Binary, Eq, Show)

-- | A notification for Retcon that the document with 'ForeignID' which is an
-- 'EntityName' at the data source 'SourceName' has changed in some way.
data ChangeNotification = ChangeNotification
    { _notificationEntity    :: EntityName
    , _notificationSource    :: SourceName
    , _notificationForeignID :: ForeignID
    }
  deriving (Eq, Show)
makeLenses ''ChangeNotification

-- | An opaque reference to a DiffOp, used when sending the list of selected
-- DiffOps to resolveDiff
newtype ConflictedDiffOpID = ConflictedDiffOpID
    { unConflictedDiffOpID :: Int }
  deriving (Binary, Eq, Show)

getJSON
    :: FromJSON a
    => Get a
getJSON = do
    json <- Aeson.eitherDecode <$> get
    case json of
        Right x -> return x
        Left msg -> fail msg

instance Binary (Diff ()) where
    put = put . Aeson.encode
    get = getJSON

instance Binary (DiffOp ()) where
    put = put . Aeson.encode
    get = getJSON

instance Binary Document where
    put = put . Aeson.encode
    get = getJSON

data RequestConflicted = RequestConflicted
  deriving (Eq, Show)
data ResponseConflicted
    = ResponseConflicted
        [ ( Document
          , Diff ()
          , DiffID
          , [(ConflictedDiffOpID, DiffOp ())]
        )]
    -- | Pre-serialised version of the same data. This is generated on the server
    -- to avoid the overhead of de-serialising from the database only to serialise
    -- immediately. Woo.
    | ResponseConflictedSerialised
        [ ( BS.ByteString
          , BS.ByteString
          , Int
          , [(Int, BS.ByteString)]
        )]
  deriving (Eq, Show)

instance Binary RequestConflicted where
    put _ = return ()
    get = return RequestConflicted
instance Binary ResponseConflicted where
    put (ResponseConflicted ds) = put ds
    put (ResponseConflictedSerialised ds) = put ds
    get = ResponseConflicted <$> get

data RequestChange = RequestChange ChangeNotification
  deriving (Eq, Show)
data ResponseChange = ResponseChange
  deriving (Eq, Show)

instance Binary RequestChange where
    put (RequestChange (ChangeNotification entity source fk)) =
        put (entity, source, fk)
    get = do
        (entity, source, fk) <- get
        return . RequestChange $ ChangeNotification entity source fk
instance Binary ResponseChange where
    put _ = return ()
    get = return ResponseChange

data RequestResolve = RequestResolve DiffID [ConflictedDiffOpID]
  deriving (Eq, Show)
data ResponseResolve = ResponseResolve
  deriving (Eq, Show)

instance Binary RequestResolve where
    put (RequestResolve did conflicts) = put (did, conflicts)
    get = do
        (did, conflicts) <- get
        return $ RequestResolve did conflicts
instance Binary ResponseResolve where
    put _ = return ()
    get = return ResponseResolve

data InvalidRequest = InvalidRequest
  deriving (Eq, Show)
data InvalidResponse = InvalidResponse
  deriving (Eq, Show)

instance Binary InvalidRequest where
    put _ = return ()
    get = return InvalidRequest
instance Binary InvalidResponse where
    put _ = return ()
    get = return InvalidResponse

-- | Request to flush and process all items from the work queue.
data RequestFlushWork = RequestFlushWork
  deriving (Eq, Show)

-- | Response to flush-and-process request.
data ResponseFlushWork = ResponseFlushWork Int
  deriving (Eq, Show)

instance Binary RequestFlushWork where
    put _ = return ()
    get   = return RequestFlushWork
instance Binary ResponseFlushWork where
    put (ResponseFlushWork n) = put n
    get = do
        n <- get
        return $ ResponseFlushWork n

-- | Message exchanged to be made in the network protocol.
data Header request response where
    HeaderConflicted :: Header RequestConflicted ResponseConflicted
    HeaderChange :: Header RequestChange ResponseChange
    HeaderResolve :: Header RequestResolve ResponseResolve
    InvalidHeader :: Header InvalidRequest InvalidResponse
    HeaderFlushWork :: Header RequestFlushWork ResponseFlushWork

-- | Existential wrapper around some -- any -- 'Header' defining a valid
-- protocol interaction.
data SomeHeader where
    SomeHeader
        :: Header request response
        -> SomeHeader

instance Enum SomeHeader where
    fromEnum (SomeHeader HeaderConflicted) = 0
    fromEnum (SomeHeader HeaderChange) = 1
    fromEnum (SomeHeader HeaderResolve) = 2
    fromEnum (SomeHeader HeaderFlushWork) = 3
    fromEnum (SomeHeader InvalidHeader) = maxBound

    toEnum 0 = SomeHeader HeaderConflicted
    toEnum 1 = SomeHeader HeaderChange
    toEnum 2 = SomeHeader HeaderResolve
    toEnum 3 = SomeHeader HeaderFlushWork
    toEnum _ = SomeHeader InvalidHeader

-- * Server configuration

-- | Configuration for the server.
data ServerConfig = ServerConfig
    { _cfgConnectionString :: String
    }
  deriving (Show, Eq)
makeLenses ''ServerConfig

-- | Parser for server options.
serverParser :: Parser ServerConfig
serverParser = ServerConfig <$> connString
  where
    connString = option str (
           long "address"
        <> short 'A'
        <> metavar "SOCKET"
        <> help "Server socket. e.g. tcp://0.0.0.0:60179")

-- * Server monad

-- | The State carried around in the server for retcon actions it needs to run.
type State = RetconMonadState InitialisedEntity RWToken ()

-- | Monad for the API server actions to run in.
newtype RetconServer z a = RetconServer
    { unRetconServer :: ExceptT RetconAPIError
        (ReaderT (Socket z Rep)
            (ReaderT State
                (LoggingT
                    (ZMQ z)))) a
    }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader (Socket z Rep),
            MonadError RetconAPIError, MonadLogger, MonadCatch, MonadThrow)

-- | Run a handler in the 'RetconServer' monad using the ZMQ connection details.
runRetconServer
    :: forall a. ServerConfig
    -> State
    -> (forall z. RetconServer z a)
    -> IO ()
runRetconServer cfg state act = runZMQ $ runStdoutLoggingT $ do
    sock <- lift . socket $ Rep
    lift . bind sock $ cfg ^. cfgConnectionString
    void $ flip runReaderT state . flip runReaderT sock . runExceptT . unRetconServer $ act

-- * Monads with ZMQ

liftZMQ :: ZMQ z a -> RetconServer z a
liftZMQ = RetconServer . lift . lift . lift . lift

askState :: (forall z. RetconServer z State)
askState = RetconServer . lift . lift $ ask

-- * Server actions

decodeStrict
    :: (MonadError RetconAPIError m, Binary a)
    => BS.ByteString
    -> m a
decodeStrict bs =
    case decodeOrFail . fromStrict $ bs of
        Left e ->
            -- TODO: Log this error somehow.
            throwError DecodeError
        Right (_, _, x) -> return x

encodeStrict
    :: (Binary a)
    => a
    -> BS.ByteString
encodeStrict = toStrict . encode

-- | Implement the API protocol.
protocol
    :: RetconServer z ()
protocol = loop
  where
    loop = do
        sock <- ask
        cmd <- liftZMQ . receiveMulti $ sock
        -- Decode and process the message.
        (status, resp) <- case cmd of
            [hdr, req] -> catchAndInject $
                          join $ dispatch <$> (toEnum <$> decodeStrict hdr)
                                          <*> pure (fromStrict req)
            _          -> throwError InvalidNumberOfMessageParts
        -- Encode and send the response.
        liftZMQ . sendMulti sock . fromList $ [encodeStrict status, resp]
        -- LOOOOOP
        loop

    -- Decode a request and call the appropriate handler.
    dispatch
        :: SomeHeader
        -> LBS.ByteString
        -> RetconServer z (Bool, BS.ByteString)
    dispatch (SomeHeader hdr) body =
        (True,) <$> case hdr of
            HeaderConflicted -> encodeStrict <$> listConflicts (decode body)
            HeaderResolve -> encodeStrict <$> resolveConflict (decode body)
            HeaderChange -> encodeStrict <$> notify (decode body)
            HeaderFlushWork -> encodeStrict <$> flushWork (decode body)
            InvalidHeader -> return . encodeStrict $ InvalidResponse

    -- Catch exceptions and inject them into the monad as errors.
    --
    -- TODO: Chain together the catching and handling of difference Exception
    -- types and return more specific errors, if available.
    catchAndInject
        :: RetconServer z (Bool, BS.ByteString)
        -> RetconServer z (Bool, BS.ByteString)
    catchAndInject act = catchError (catch act injectSomeException) reportAPIError
      where
        injectSomeException
            :: (MonadLogger m, MonadError RetconAPIError m)
            => SomeException
            -> m a
        injectSomeException e = do
            logErrorN . fromString $
                "Intercepted error to forward to client: " <> show e
            throwError UnknownServerError

    -- Handle an error in executing operations by sending it back to the client.
    reportAPIError
        :: RetconAPIError
        -> RetconServer z (Bool, BS.ByteString)
    reportAPIError e = do
        logErrorN . fromString $
            "Could not process message: " <> show e
        return (False, toStrict . encode . fromEnum $ e)

-- | Run a RetconMonad action in the RetconServer monad.
runRetconMonadInServer
    :: RetconMonad InitialisedEntity RWToken () r
    -> RetconServer z r
runRetconMonadInServer act = do
    state <- askState
    result <- liftIO . runRetconMonad state $ act

    case result of
        Left (RetconUnknown key) -> do
            logErrorN . fromString $
                "Client notified an unknown key: " <> key
            throwError UnknownKeyError
        Left e -> do
            logErrorN . fromString $
                "Error running retcon action in server: " <> show e
            throwError UnknownServerError
        Right r -> return r

-- | Process a _notify_ message from the client, checking the
notify
    :: RequestChange
    -> RetconServer z ResponseChange
notify (RequestChange (ChangeNotification n d i)) = do
    logInfoN . fromString $
        "Processing change notification: " <> show (n,d,i)

    runRetconMonadInServer $ do
        -- Check that the notification details are valid and, if so, add to the
        -- work queue.
        entities <- map dropEntityState <$> getRetconState
        case someForeignKey entities (n,d,i) of
            Just _  -> addWork (WorkNotify (n,d,i))
            Nothing -> throwError (RetconUnknown $ show (n,d,i))

    return ResponseChange

-- | Process a _resolve conflict_ message from the client.
--
-- The selected diff is marked as resolved; and a new diff is composed from the
-- selected operations and added to the work queue.
resolveConflict
    :: RequestResolve
    -> RetconServer z ResponseResolve
resolveConflict (RequestResolve diff_id op_ids) = do
    logInfoN . fromString $
        "Resolving diff " <> show (unDiffID diff_id) <> " with " <>
        (show . map unConflictedDiffOpID $ op_ids)

    runRetconMonadInServer $ do
        new_diff <- composeNewDiff op_ids
        addWork (WorkApplyPatch (unDiffID diff_id) new_diff)

    return ResponseResolve

-- | Fetch the details of outstanding conflicts and return them to the client.
listConflicts
    :: RequestConflicted
    -> RetconServer z ResponseConflicted
listConflicts RequestConflicted = do
    logInfoN . fromString $
        "Listing conflicts for client"

    conflicts <- runRetconMonadInServer lookupConflicts

    logInfoN . fromString $
        "Found " <> show (Prelude.length conflicts) <> " conflicts!"

    return $ ResponseConflictedSerialised conflicts

-- | Flush and process all work items in the work queue.
flushWork
    :: RequestFlushWork
    -> RetconServer z ResponseFlushWork
flushWork RequestFlushWork = do
    logInfoN . fromString $
        "Flushing work items from work queue."

    -- Process the work items.
    n <- runRetconMonadInServer $ do
        result <- processWork processWorkItem
        case result of
            Just _ -> return 1
            Nothing -> return 0

    logDebugN . fromString $
        "Flushed " <> show n <> " work items."

    return $ ResponseFlushWork n

-- | Retrieve selected 'DiffOp's from the store and combine them into a new
-- 'Diff'.
composeNewDiff
    :: ReadableToken s
    => [ConflictedDiffOpID]
    -> RetconMonad e s l (Diff ())
composeNewDiff op_ids = do
    ops <- lookupDiffConflicts . map unConflictedDiffOpID $ op_ids
    return $ Diff () $ map thrd ops
  where
    thrd (_, _, c) = c

-- * API server

-- | Start a server running the retcon API over a ZMQ socket.
--
-- Open a ZMQ_REP socket and receive requests from it; unhandled errors are caught
-- and fed back through the socket to the client.
apiServer
    :: RetconConfig SomeEntity RWToken
    -> ServerConfig
    -> IO ()
apiServer retcon_cfg server_cfg = runLogging (retcon_cfg ^. cfgLogging) $ do
    logInfoN . fromString $
        "Running server on " <> server_cfg ^. cfgConnectionString

    -- Start the threads
    servert <- liftIO . async $ bracket newState finaliseRetconState serverThread
    retcont <- liftIO . async $ bracket newState finaliseRetconState retconThread
    liftIO . flip catch (cleanup servert retcont) $ do
        void $ waitEitherCancel servert retcont
        void . waitCatch $ servert
        void . waitCatch $ retcont

    logDebugN . fromString $
        "Started API server and processing thread."
  where
    cleanup :: Async a -> Async b -> E.AsyncException -> IO ()
    cleanup t1 t2 _ = do
        cancel t1
        cancel t2
        void $ waitBoth t1 t2
    newState =
        initialiseRetconState retcon_cfg ()
    serverThread state = do
        putStrLn . fromString $
          "Starting server, handling " <> (show . length $ state ^. retconConfig . cfgEntities) <> " entities"
        void $ runRetconServer server_cfg state protocol
        putStrLn  . fromString $
          "Done server"
    retconThread state = do
        putStrLn . fromString $
          "Starting worker, handling " <>
          (show . length $ state ^. retconConfig . cfgEntities) <>
          " entities"
        void . runRetconMonad state . forever $ do
            work <- processWork processWorkItem
            case work of
                Just _  -> return ()
                Nothing -> do
                    liftIO . threadDelay $ 50000
        putStrLn "Done worker"

-- | Inspect a work item and perform whatever task is required.
processWorkItem
    :: WritableToken s
    => WorkItem
    -> RetconHandler s ()
processWorkItem work = do
    whenVerbose . logInfoN . fromString $
        "Processing work item: " <> show work
    case work of
        WorkNotify fkval -> do
            logDebugN . fromString $
                "Processing a notifcation: " <> show fkval
            dispatch fkval

        WorkApplyPatch did new_diff -> do
            logDebugN . fromString $
                "Processing a diff: " <> show did

            details <- lookupDiff did
            case details of
                Nothing -> do
                    logErrorN . fromString $
                        "Could not process diff " <> show did <>
                        " because it is missing!"
                    return ()
                Just (SomeInternalKey ik, _, _) -> do
                    -- TODO: Check that the diff is conflicting.

                    -- Get the documents from the data sources.
                    docs <- getDocuments ik

                    -- Get or build and initial document.
                    initial <- fromMaybe (calculateInitialDocument . rights $ docs) <$>
                               lookupInitialDocument ik

                    -- Apply the diff and save the altered documents.
                    distributeDiff ik initial (new_diff, [])
                        . map (either (const initial) id)
                        $ docs

                    -- Mark the diff as being resolved.
                    resolveDiff did
    return ()
