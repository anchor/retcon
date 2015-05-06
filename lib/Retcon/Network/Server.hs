{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Retcon.Network.Server where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Error.Util         ()
import qualified Control.Exception          as E
import           Control.Lens               hiding (Context, coerce)
import           Control.Monad.Catch
import           Control.Monad.Error.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import qualified Data.Aeson.Diff            as D
import           Data.Binary
import qualified Data.ByteString            as BS hiding (unpack)
import qualified Data.ByteString.Char8      as BS (unpack)
import           Data.ByteString.Lazy       (fromStrict, toStrict)
import qualified Data.ByteString.Lazy       as LBS
import           Data.Coerce
import           Data.Either
import qualified Data.List                  as L
import           Data.List.NonEmpty         hiding (filter, length, map)
import qualified Data.Map                   as M
import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Traversable           ()
import           System.Log.Logger
import           System.ZMQ4

import           Retcon.Configuration
import           Retcon.DataSource          (runDSMonad)
import qualified Retcon.DataSource          as DS
import           Retcon.Diff
import           Retcon.Document
import           Retcon.Identifier
import           Retcon.Monad
import           Retcon.Network.Protocol
import           Retcon.Store
import           Retcon.Store.PostgreSQL


type ErrorMsg = String

--------------------------------------------------------------------------------

-- * Server

data ServerState = ServerState
    { _serverContext :: Context       -- ^ ZMQ context
    , _serverSocket  :: Socket Rep    -- ^ ZMQ socket
    , _serverConfig  :: Configuration -- ^ retcond config
    , _serverStore   :: PGStore       -- ^ Internal data store, shared between all server threads
    }
makeLenses ''ServerState

-- | Name of server component for logging.
logName :: String
logName = "Retcon.Server"

-- | Spawn a thread serving the retcon API and a number of worker threads
--   to process requests accepted by that server.
--
spawnServer :: Configuration -> Int -> IO ()
spawnServer cfg n = do
  _ <- bracket start stop $ \state -> do

      -- Spawn a server implementing the protocol and some workers
      api      <- spawnServerAPI state
      peasants <- spawnServerWorkers state

      -- Ensures workers die if the API server does.
      mapM_ (link2 api) peasants

      -- Wait for all of the API server or worker threads to finish.
      mapM_ wait (api:peasants)
  return ()
  where
    spawnServerAPI :: ServerState -> IO (Async ())
    spawnServerAPI = async . flip runProtocol protocol

    spawnServerWorkers :: ServerState -> IO [Async ()]
    spawnServerWorkers state
      = replicateM n (async $ worker (_serverStore state) cfg)

    start :: IO ServerState
    start = do
        noticeM logName "Starting Server"
        let (zmq_conn, _, pg_conn) = configServer cfg
        ctx    <- context
        sock   <- socket ctx Rep
        bind sock zmq_conn
        db     <- initBackend (PGOpts pg_conn)
        return $  ServerState ctx sock cfg db

    stop :: ServerState -> IO ()
    stop state = do
        closeBackend $ state ^. serverStore
        close        $ state ^. serverSocket
        term         $ state ^. serverContext
        noticeM logName "Stopped Server"

--------------------------------------------------------------------------------

-- * Protocol Implementation

-- | A monad which wraps up some state, some error handling, and some IO to
-- implement the server side of retcond.
newtype Protocol a = Proto
    { unProtocol :: ExceptT APIError (ReaderT ServerState IO) a }
  deriving (Applicative, Functor, Monad, MonadError APIError,
  MonadIO, MonadReader ServerState)

instance MonadThrow Protocol where
    throwM = liftIO . E.throwIO

instance MonadCatch Protocol where
    (Proto (ExceptT m)) `catch` f = Proto . ExceptT $ m `catch` (runExceptT . unProtocol . f)

-- | Execute a 'Protocol' action.
runProtocol :: ServerState -> Protocol a -> IO a
runProtocol s act = do
    res <- flip runReaderT s . runExceptT . unProtocol $ act
    case res of
        Left e -> throwM e
        Right a -> return a

-- | Server protocol handler.
protocol :: Protocol ()
protocol = loop
  where
    loop = do
        sock <- _serverSocket <$> ask
        -- Read a response from the client.
        cmd <- liftIO $ receiveMulti sock
        -- Decode and process it.
        (status, resp) <- case cmd of
            [hdr, req] -> catchAndInject . join $
                    dispatch <$> (toEnum <$> decodeStrict hdr)
                             <*> pure (fromStrict req)
            _ -> throwError InvalidNumberOfMessageParts
        -- Send the response to the client.
        liftIO . sendMulti sock . fromList $ [encodeStrict status, resp]
        -- Play it again, Sam.
        loop
    dispatch
        :: SomeHeader
        -> LBS.ByteString
        -> Protocol (Bool, BS.ByteString)
    dispatch (SomeHeader hdr) body =
        (True,) <$> case hdr of
            HeaderConflicted -> encodeStrict <$> listConflicts (decode body)
            HeaderResolve    -> encodeStrict <$> resolveConflict (decode body)
            HeaderChange     -> encodeStrict <$> notify (decode body)
            InvalidHeader    -> return . encodeStrict $ InvalidResponse

    -- Catch exceptions and inject them into the monad as errors.
    --
    -- TODO: Chain together the catching and handling of difference Exception
    -- types and return more specific errors, if available.
    catchAndInject
        :: Protocol (Bool, BS.ByteString)
        -> Protocol (Bool, BS.ByteString)
    catchAndInject act = catchError (catch act injectSomeException) reportAPIError
      where
        injectSomeException
            :: (MonadIO m, MonadError APIError m)
            => SomeException
            -> m a
        injectSomeException e = do
            liftIO . errorM logName . fromString $
                "Intercepted error to forward to client: " <> show e
            throwError UnknownServerError

    -- Handle an error in executing operations by sending it back to the client.
    reportAPIError
        :: APIError
        -> Protocol (Bool, BS.ByteString)
    reportAPIError e = do
        liftIO . errorM logName . fromString $
            "Could not process message: " <> show e
        return (False, toStrict . encode . fromEnum $ e)

-- | Process a request for unresolved conflicts.
listConflicts
    :: RequestConflicted
    -> Protocol ResponseConflicted
listConflicts RequestConflicted = do
    liftIO $ infoM logName "Listing conflicts"
    conflicts <- liftIO . lookupConflicts =<< view serverStore
    return $ ResponseConflictedSerialised $ fmap mkRespItem conflicts
    where mkRespItem ConflictResp{..}
            = ResponseConflictedSerialisedItem
              _conflictRawDoc
              _conflictRawDiff
              (coerce _conflictDiffID)
              (coerce _conflictRawOps)

-- | Process and resolve a conflict.
resolveConflict
    :: RequestResolve
    -> Protocol ResponseResolve
resolveConflict (RequestResolve diffID opIDs) = do
    store <- view serverStore
    liftIO . infoM logName $ "Resolving conflict: " <> show diffID
    new <- composeNewDiff store
    liftIO $ addWork store (WorkApplyPatch diffID $ new ^. patchDiff)
    return ResponseResolve
    where
      composeNewDiff store = do
        things <- liftIO $ lookupDiffConflicts store opIDs
        return $ Patch Unamed $ D.Patch $ map _ops things

-- | Notification of a change to be queued for processing.
notify
    :: RequestChange
    -> Protocol ResponseChange
notify (RequestChange nt) = do
    let ent_name = nt ^. notificationEntity
        src_name = nt ^. notificationSource
        fid      = nt ^. notificationForeignID
    cfg   <- _serverConfig <$> ask
    store <- _serverStore  <$> ask

    liftIO . infoM logName . T.unpack $
        "Received change notification for: " <> ename ent_name <> "." <>
        sname src_name <> "/" <> fid

    let m_ds = do
            Entity{..} <- M.lookup ent_name (configEntities cfg)
            M.lookup src_name entitySources

    case m_ds of
        Nothing -> do
            liftIO . errorM logName . T.unpack $ "Unknown entity or source: "
                <> ename ent_name <> "." <> sname src_name
            throwError UnknownKeyError
        Just _ ->
            liftIO . addWork store . WorkNotify $ ForeignKey ent_name src_name fid

    return ResponseChange


--------------------------------------------------------------------------------

-- * Asynchronous Server Workers

-- | Get a work item from the work queue, apply a constant backoff if there is
--   nothing in the queue.
--
--   This function must not
getWorkBackoff :: Store store => store -> IO (WorkItemID, WorkItem)
getWorkBackoff store = do
  work <- getWork store
  case work of
    Nothing -> threadDelay 50000 >> getWorkBackoff store
    Just x  -> return x

-- | A worker for the retcond server.
--   These workers cannot die, they simply log any errors and keep going.
--
worker :: Store store => store -> Configuration -> IO ()
worker store cfg = go
  where -- Get a work item from the queue, mark it as busy and try to complete it.
        -- If all goes well, mark the work as finished when done, otherwise signal
        -- it as free.
        --
        go      = do
          bracketOnError getIt ungetIt completeIt
          go

        getIt   = liftIO $ getWorkBackoff store
        ungetIt = ungetWork store . fst

        completeIt (work_id, item) = do
          case item of
            WorkNotify fk -> do
              liftIO . debugM logName $ "Processing a notifcation: " <> show fk
              processNotification store cfg fk
            WorkApplyPatch did a_diff -> do
              liftIO . debugM logName $ "Processing a diff: " <> show did
              processDiff store cfg did a_diff
          completeWork store work_id

-- notifications

processNotification :: Store store => store -> Configuration -> ForeignKey -> IO ()
processNotification store cfg fk@(ForeignKey{..}) = do
  let x = do e <- M.lookup fkEntity (configEntities cfg)
             d <- M.lookup fkSource (entitySources e)
             return (e, d)
  case x of
    Nothing -> liftIO . criticalM logName $ "Unknown key in workqueue: " <> show fk
    Just (entity, source) -> do
      ik    <- liftIO     $ lookupInternalKey store fk
      doc   <- runDSMonad $ DS.readDocument source fk

      let allSources = M.elems (entitySources entity)
          sources    = L.delete source allSources

      liftIO $ case (ik, doc) of
        (Nothing, Left  e) -> notifyProblem (RetconUnknown $ show e)
        (Nothing, Right d) -> notifyCreate  store sources fk d
        (Just i,  Left  _) -> notifyDelete  store sources i
        (Just i,  Right _) -> notifyUpdate  store allSources i (entityPolicy entity)

-- | Creates a new internal document to reflect a new foreign change. Update
--   all given data sources of the change.
--
--   Caller is responsible for: ensuring the datasources exclude the one from
--   which the event originates.
--
notifyCreate :: Store store => store -> [DataSource] -> ForeignKey -> Document -> IO ()
notifyCreate store datasources fk@(ForeignKey{..}) doc@(Document{..}) = do
  infoM logName $ "CREATE: " <> show fk

  -- Create an internal key associated with the new document
  ik  <- createInternalKey store fkEntity
  recordForeignKey store ik fk

  -- Create an initial document
  recordInitialDocument store ik doc

  -- Update other sources in the entity
  forM_ datasources (createDoc ik)

  where createDoc ik ds = do
          x <- runDSMonad
             $ DS.createDocument ds
             $ graftDoc doc ds
          case x of
            Left  e -> errorM logName ("notifyCreate: " <> show e)
            Right f -> recordForeignKey store ik f

        graftDoc Document{..} DataSource{..}
          = Document sourceEntity sourceName _documentContent

-- | Deletes internal document to reflect the foreign change. Update
--   all given data sources of the change.
--
--   Caller is responsible for: ensuring the datasources exclude the one from
--   which the event originates.
--
notifyDelete
  :: Store store => store
  -> [DataSource]
  -> InternalKey
  -> IO ()
notifyDelete store datasources ik = do
  infoM logName $ "DELETE: " <> show ik
  forM_ datasources deleteDoc

  where deleteDoc ds = do
          f <- lookupForeignKey store (sourceName ds) ik
          case f of
            Nothing -> return ()
            Just fk -> do
              -- Delete the document
              hushBoth $ runDSMonad $ DS.deleteDocument ds fk
              -- Delete known foreign key
              void $ deleteForeignKey store fk
          -- Delete internal bookkeeping
          void $ deleteInitialDocument store ik
          deleteInternalKey store ik

-- | Updates internal document to reflect the foreign change. Update
--   all given data sources of the change.
--
notifyUpdate
  :: Store store => store
  -> [DataSource]
  -> InternalKey
  -> MergePolicy
  -> IO ()
notifyUpdate store datasources ik policy = do
  infoM logName $ "UPDATE: " <> show ik

  -- Fetch documents from all data sources.
  docs <- mapM (getDocument store ik) datasources
  let (_missing, valid) = partitionEithers docs

  -- Load (or calculate) the initial document.
  initial <- lookupInitialDocument store ik >>=
      maybe (calculate valid) return

  -- Extract and merge patches.
  let (merged, rejects) = mergeAll policy $ map (diff policy initial) valid

  if   null rejects
  then debugM logName $ "No rejected changes processing " <> show ik
  else infoM  logName $ "Rejected " <> show (length rejects) <> " changes in " <> show ik

  -- Record changes in history.
  did <- recordDiffs store ik (merged, rejects)
  infoM logName $ "Recorded diff " <> show did <> " against " <> show ik

  -- Update and save the documents.
  let docs' = map (patch policy merged . either (const initial) id) docs
  mapM_ (setDocument store ik) (L.zip datasources docs')

  -- Update initial document.
  let initial' = patch policy merged initial
  recordInitialDocument store ik initial'

  where
    calculate :: [Document] -> IO Document
    calculate docs = do
      infoM logName $ "No initial document for " <> show ik <> "."
      return . either (const $ emptyDocument (ikEntity ik) "<initial>") id
        $ calculateInitialDocument docs

-- | Logs a problem with the notification.
notifyProblem :: RetconError -> IO ()
notifyProblem = errorM logName . (<>) "notifyProblem: " . show

-- | Apply a 'Patch' to resolve the conflicts on a previous update.
--
-- TODO(thsutton) We need to check the diff hasn't already been resolved.
processDiff
    :: (Store store, MonadIO m, Functor m)
    => store
    -> Configuration
    -> DiffID
    -> D.Patch
    -> m ()
processDiff store cfg diff_id a_diff = do
  res <- runExceptT act
  case res of
    Left e -> liftIO . errorM logName $ e
    Right () -> return ()
  where
    act = do
      liftIO . infoM logName $ "Resolving errors in diff " <> show diff_id
      conflict <- getConflict

      let en = EntityName . T.decodeUtf8 $ conflict ^. diffEntity
          ik = InternalKey en $ conflict ^. diffKey
          a_patch = Patch Unamed a_diff

      (policy, srcs) <- getSources en

      -- 0. Load and update the initial document.
      initial <- liftIO $ fromMaybe (emptyDocument en "<initial>") <$>
        lookupInitialDocument store ik
      let initial' = patch policy a_patch initial

      -- 1. Apply the patch to all sources.
      mapM_ (\src -> liftIO $ do
        doc <- either (const initial') id <$> getDocument store ik src
        setDocument store ik (src, patch policy a_patch doc)
        ) srcs

      -- 2. Record the updated initial document.
      liftIO $ recordInitialDocument store ik initial'

      -- 3. Mark the conflicted patch as resolved.
      liftIO $ resolveDiffs store diff_id
      return ()

    getConflict = do
        conf <- liftIO $ lookupDiff store diff_id
        case conf of
          Nothing -> throwError $
            "Cannot resolve diff " <> show diff_id <> " because it doesn't exist."
          Just v -> return v

    getSources en = do
      let things = do
            e    <- M.lookup en (configEntities cfg)
            return ( entityPolicy e
                   , map snd . M.toList . entitySources $ e)
      case things of
        Nothing -> throwError $
            "Cannot resolve diff " <> show diff_id <> " because there are no "
            <> "sources for " <> show en <> "."
        Just x  -> return x

--------------------------------------------------------------------------------

-- * Data source functions

-- | Get the 'Document' corresponding to an 'InternalKey' from a 'DataSource'.
getDocument
    :: Store store
    => store
    -> InternalKey
    -> DataSource
    -> IO (Either ErrorMsg Document)
getDocument store ik ds = do
  f  <- lookupForeignKey store (sourceName ds) ik
  case f of
    Nothing -> return (Left "getDocument: No foreign key found.")
    Just fk -> fmap (over _Left show) . DS.runDSMonad $ DS.readDocument ds fk

-- | Set the 'Document' in the given 'DataSource' corresponding to an 'InternalKey'.
setDocument
    :: Store store
    => store
    -> InternalKey
    -> (DataSource, Document)
    -> IO (Either ErrorMsg ForeignKey)
setDocument store ik (ds, doc) = do
  f <- lookupForeignKey store (sourceName ds) ik
  case f of
    Nothing -> return (Left "setDocument: No foreign key found.")
    Just fk -> fmap (over _Left show) . DS.runDSMonad $ DS.updateDocument ds fk doc

-- | Merge a sequence of 'Patch'es by applying a 'MergePolicy'.
--
mergeAll
    :: MergePolicy
    -> [Patch]
    -> (Patch, [RejectedOp])
mergeAll pol =
  foldr (\p1 (p2, r) -> (r <>) <$> merge pol p1 p2)
        (emptyPatch, mempty)

extractDiff
    :: Document
    -> Document
    -> Patch
extractDiff = diff ignoreConflicts


--------------------------------------------------------------------------------

-- * Utility functions

-- | Decode a serializable value from a strict 'ByteString'.
--
-- If the bytestring cannot be decoded, a 'DecodeError' is thrown.
decodeStrict
    :: (MonadIO m, MonadError APIError m, Binary a)
    => BS.ByteString
    -> m a
decodeStrict bs =
    case decodeOrFail . fromStrict $ bs of
        Left _ -> do
            liftIO . warningM logName . BS.unpack $
                "Decode failure for input: " <> bs
            throwError DecodeError
        Right (_, _, x) -> return x

-- | Encode a serialisable value into a strict 'ByteString'.
encodeStrict
    :: (Binary a)
    => a
    -> BS.ByteString
encodeStrict = toStrict . encode

-- | Silences both errors (via logging) and results.
--
hushBoth :: Show a => IO (Either a b) -> IO ()
hushBoth act = act >>= \x -> case x of
  Left e  -> errorM logName (show e)
  Right _ -> return ()
