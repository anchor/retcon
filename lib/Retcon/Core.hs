--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

-- | The "core" of recton, all concrete types and classes that pull together
-- modules and helpers to define data sources and entities.
module Retcon.Core
(
    -- * RetconMonad type aliases
    RetconAction,
    RetconHandler,

    -- * Action runners
    runRetconAction,
    runRetconMonadOnce,

    -- * State helpers
    initialiseRetconState,
    finaliseRetconState,

    -- * Entities
    RetconEntity(..),

    -- * Data sources
    Initialiser(..),
    runInitialiser,

    RetconDataSource(..),

    accessState,
    initialiseEntities,
    finaliseEntities,
    initialiseSources,
    finaliseSources,

    -- * Wrapper types
    SomeEntity(..),
    someEntityName,
    someEntityNames,

    SomeDataSource(..),
    someDataSourceName,

    InitialisedEntity(..),
    InitialisedSource(..),

    dropEntityState,

    -- * Keys
    InternalKey(..),
    SomeInternalKey(..),
    ForeignKey(..),
    SomeForeignKey(..),

    EntityName,
    SourceName,
    InternalID,
    ForeignID,

    InternalKeyIdentifier,
    ForeignKeyIdentifier,

    internalKeyValue,
    someInternalKey,
    encodeForeignKey,
    foreignKeyValue,
    someForeignKey,

    WorkItem(..),

    -- * Internal stores for operational data
    RetconStore(..),
    token,

    -- * Tokens, for access control of internal stores
    StoreToken(..),
    ReadableToken(..),
    WritableToken(..),
    RWToken,
    ROToken,
) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Exception.Enclosed
import qualified Control.Exception.Lifted as LE
import Control.Lens (view)
import Control.Lens.Operators
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import Data.Biapplicative
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.Type.Equality
import GHC.TypeLits

import Retcon.Diff
import Retcon.Document
import Retcon.Error
import Retcon.Monad
import Retcon.Notifications
import Retcon.Options
import Utility.Configuration

-- | Restricted monad with read-only access to the retcon storage.
type RetconAction l a = RetconMonad InitialisedEntity ROToken l a

type RetconHandler s a = RetconMonad InitialisedEntity s () a

-- | Run an action in the 'RetconAction' monad (aka the 'RetconMonad' with
-- read-only storage).
--
-- Errors which occur in the action are handled and will not propagate up into
-- the parent handler.
runRetconAction :: StoreToken s
                => l
                -> RetconAction l a
                -> RetconHandler s (Either RetconError a)
runRetconAction l =
    -- Restrict store and add the local state.
    RetconMonad . withReaderT (localise l) . unRetconMonad .
    -- Do exception and error handling.
    handle . handleAny (throwError . RetconError)
  where
    -- | Handle any errors in an action, pulling them into the monad.
    handle :: (Functor m, MonadError e m) => m v -> m (Either e v)
    handle a = (Right <$> a) `catchError` (return . Left)

-- | "Localise" the state as appropriate to run a 'RetconAction' in a
-- 'RetconHandler' context.
localise
    :: StoreToken s
    => l -- ^ Local state value
    -> RetconMonadState e s () -- ^ Handler state
    -> RetconMonadState e ROToken l
localise local_state state =
    state & retconConfig . cfgDB %~ restrictToken
          & localState .~ local_state

initialiseRetconState
    :: RetconConfig SomeEntity s
    -> l
    -> IO (RetconMonadState InitialisedEntity s l)
initialiseRetconState cfg l = do
    let params   = cfg ^. cfgParams
        entities = cfg ^. cfgEntities
    state <- initialiseEntities params entities
    let cfg' = cfg & cfgEntities .~ state
    return $ RetconMonadState cfg' l

finaliseRetconState
    :: RetconMonadState InitialisedEntity s l
    -> IO (RetconMonadState SomeEntity s l)
finaliseRetconState (RetconMonadState cfg l) = do
    let params = cfg ^. cfgParams
    putStrLn "finalising state"
    entities <- finaliseEntities params $ cfg ^. cfgEntities
    return $ RetconMonadState (cfg & cfgEntities .~ entities) l

-- | Execute an action in the 'RetconMonad' monad with configuration
-- initialized and finalized.
runRetconMonadOnce
    :: RetconConfig SomeEntity s
    -> l
    -> RetconMonad InitialisedEntity s l a
    -> IO (Either RetconError a)
runRetconMonadOnce cfg l action =
    LE.bracket (initialiseRetconState cfg l)
               (void . finaliseRetconState)
               (`runRetconMonad` action)

-- | The 'RetconEntity' type class associates a 'Symbol' identifying a
-- particular entity (i.e. a type of data) with a list of 'RetconDataSource's
-- which deal in that entity.
--
-- An implementation should look something like this:
--
-- > instance RetconEntity "account" where
-- >     entitySource _ = [SomeDataSource (Proxy :: "customer-api")]
--
class (KnownSymbol entity) => RetconEntity entity where
    -- | Get a list of data sources associated with the entity.
    entitySources :: Proxy entity -> [SomeDataSource entity]


-- | Monad for initialisers.
newtype Initialiser s a = Initialiser
    { unInitialiser :: ReaderT s IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader s, MonadBase IO)

-- | Convenient name for the 'Initialiser' for data sources.
type DataSourceInit a = Initialiser (Map Text Text) a

-- | Run an 'Initialiser' action.
--
-- TODO: Catch & report exceptions in initialisation. This should probably tear
-- down the world?
runInitialiser :: s -> Initialiser s a -> IO a
runInitialiser s (Initialiser a) = runReaderT a s

-- | The 'RetconDataSource' type class associates two 'Symbol' types: the first
-- identifies an entity (i.e. a type of data) and the second identifies a
-- system which handles data of that type.
--
-- Each instances provides operations allowing retcon to get, set, delete
-- 'Document' values of the appropriate sort from the external system.
class (KnownSymbol source, RetconEntity entity) => RetconDataSource entity source where

    -- | Type of state used by the data source.
    data DataSourceState entity source

    -- | Initialise the state to be used by the data source.
    --
    -- This is called during startup to, for example, open a connection to a
    -- datasource-specific database server.
    initialiseState :: DataSourceInit (DataSourceState entity source)

    -- | Finalise the state used by the data source.
    --
    -- This is called during a clean shutdown to, for example, cleanly close a
    -- database connection, etc.
    finaliseState :: DataSourceState entity source
                  -> DataSourceInit ()

    -- | Put a document into a data source.
    --
    -- If the 'ForeignKey' is not known, it will be omitted and the data source
    -- should treat the 'Document' as being newly created. In either case, the
    -- correct 'ForeignKey' for the 'Document' is returned.
    --
    -- If the document cannot be saved an error is returned in the 'Retcon'
    -- monad.
    setDocument :: Document
                -> Maybe (ForeignKey entity source)
                -> RetconAction (DataSourceState entity source) (ForeignKey entity source)

    -- | Retrieve a document from a data source.
    --
    -- If the document cannot be retrieved an error is returned in the 'Retcon'
    -- monad.
    getDocument :: ForeignKey entity source
                -> RetconAction (DataSourceState entity source) Document

    -- | Delete a document from a data source.
    --
    -- If the document cannot be deleted an error is returned in the 'Retcon'
    -- monad.
    deleteDocument :: ForeignKey entity source
                   -> RetconAction (DataSourceState entity source) ()

-- | Get the state, if any, associated with a data source.
--
-- This function will, through the judicious application of magic, determine if
-- a list of initialised entity state values (each containing initialised data
-- source state values) contains a state value for a specific data source.
--
-- Using 'foldl' here is pretty silly -- we should short circuit, etc. -- but
-- the data to be traversed will allways be short, so it doesn't matter too
-- much.
accessState :: forall e d. (RetconDataSource e d)
            => [InitialisedEntity] -- ^ Initialised state
            -> Proxy e -- ^ Entity to look for
            -> Proxy d -- ^ Data source to look for
            -> Maybe (DataSourceState e d) -- ^ State for (e,d)
accessState state entity source = foldl findEntity Nothing state
  where
    findEntity :: Maybe (DataSourceState e d)
               -> InitialisedEntity
               -> Maybe (DataSourceState e d)
    findEntity Nothing InitialisedEntity{..} =
        case sameSymbol entityProxy entity of
            Just Refl -> foldl findSource Nothing entityState
            Nothing   -> Nothing
    findEntity r       _ = r

    findSource :: Maybe (DataSourceState e d)
               -> InitialisedSource e
               -> Maybe (DataSourceState e d)
    findSource Nothing InitialisedSource{..} =
        case sameSymbol sourceProxy source of
            Just Refl -> Just sourceState
            Nothing   -> Nothing
    findSource r       _ = r

-- | Initialise the states for a collection of entities.
initialiseEntities :: ParamMap
                   -> [SomeEntity]
                   -> IO [InitialisedEntity]
initialiseEntities params = mapM initialiseEntity
  where
    initialiseEntity :: SomeEntity -> IO InitialisedEntity
    initialiseEntity (SomeEntity (p :: Proxy e)) = do
        ss <- initialiseSources params $ entitySources p
        return $ InitialisedEntity p ss

-- | Finalise the states for a collection of entities.
finaliseEntities :: ParamMap
                 -> [InitialisedEntity]
                 -> IO [SomeEntity]
finaliseEntities params = mapM finaliseEntity . reverse
  where
    finaliseEntity (InitialisedEntity p s) = do
        _ <- finaliseSources params $ reverse s
        return $ SomeEntity p

-- | Initialise the states for a collection of data sources.
initialiseSources :: forall e. RetconEntity e
                 => ParamMap
                 -> [SomeDataSource e]
                 -> IO [InitialisedSource e]
initialiseSources params = mapM initialiseSource
  where
    initialiseSource :: SomeDataSource e
                     -> IO (InitialisedSource e)
    initialiseSource ds@(SomeDataSource (p :: Proxy s) :: SomeDataSource e) =
        do
            let names = (T.pack, T.pack) <<*>> someDataSourceName ds
            let param = M.findWithDefault mempty names params
            s <- runInitialiser param initialiseState
            return $ InitialisedSource p s

-- | Finalise the states for a collection of data sources.
finaliseSources :: forall e. RetconEntity e
                 => ParamMap
                 -> [InitialisedSource e]
                 -> IO [SomeDataSource e]
finaliseSources params = mapM finaliseSource
  where
    finaliseSource :: InitialisedSource e -> IO (SomeDataSource e)
    finaliseSource (InitialisedSource p s) = do
        let ds = SomeDataSource p
        let names = (T.pack, T.pack) <<*>> someDataSourceName ds
        let param = M.findWithDefault mempty names params
        runInitialiser param $ finaliseState s
        return ds



-- $ 'Proxy' values for instances of our 'RetconEntity' and 'RetconDataSource'
-- type classes can be wrapped with existential types, allowing us to put them
-- into data structures like lists easily.
--
-- We also have wrappers which include the initialised 'DataSourceState' values
-- associated with each data source.

-- | Wrap an arbitrary 'RetconEntity'.
data SomeEntity = forall e. (KnownSymbol e, RetconEntity e) =>
    SomeEntity (Proxy e)

-- | Extract the [hopefully] human-readable name from a 'SomeEntity' value.
someEntityName :: SomeEntity
               -> String
someEntityName (SomeEntity proxy) = symbolVal proxy

-- | Extract the human-readable name of an entity and its data sources from a
-- 'SomeEntity' value.
someEntityNames :: SomeEntity
                -> (String, [String])
someEntityNames (SomeEntity entity) =
    let en = symbolVal entity
        ds = map (snd . someDataSourceName) . entitySources $ entity
    in (en, ds)

-- | Wrap an arbitrary 'RetconDataSource' for some entity 'e'.
data SomeDataSource e = forall s. RetconDataSource e s =>
    SomeDataSource (Proxy s)

-- | Extract the [hopefully] human-readable name from a 'SomeDataSource' value.
someDataSourceName :: forall e. (RetconEntity e)
                   => SomeDataSource e
                   -> (String, String)
someDataSourceName (SomeDataSource proxy) =
    (symbolVal (Proxy :: Proxy e), symbolVal proxy)

-- | Wrap an arbitrary 'RetconEntity', together with the initialised state for
-- it's sources.
data InitialisedEntity = forall e. (RetconEntity e) =>
    InitialisedEntity { entityProxy :: Proxy e
                      , entityState :: [InitialisedSource e]
                      }

-- | Convert an 'InitialisedEntity' value into a 'SomeEntity' value by
-- discarding the state.
--
-- This is a "cast" and not a finaliser; it should be used only to avoid
-- writing two versions of a function like `massageLookupDiff` below.
dropEntityState
    :: InitialisedEntity
    -> SomeEntity
dropEntityState (InitialisedEntity p _) = SomeEntity p

-- | Wrap an arbitrary 'RetconDataSource' for some entity 'e', together with
-- it's initialised state.
data InitialisedSource e = forall s. RetconDataSource e s =>
    InitialisedSource { sourceProxy :: Proxy s
                      , sourceState :: DataSourceState e s
                      }
-- $ The various parts of retcon refer to documents using two types of key
-- values: an 'InternalKey entity' identifies a 'Document' for a whole entity
-- and a 'ForeignKey entity source' identifies a 'Document' in a particular
-- data source.

-- | The unique identifier used to identify a unique 'entity' document within
-- retcon.
newtype RetconEntity entity => InternalKey entity =
    InternalKey { unInternalKey :: Int }
  deriving (Eq, Ord)

instance RetconEntity entity => Show (InternalKey entity) where
    show = show . internalKeyValue

-- | An existential wrapper around an 'InternalKey'.
data SomeInternalKey = forall entity. RetconEntity entity =>
    SomeInternalKey (InternalKey entity)

-- | The unique identifier used by the 'source' data source to refer to an
-- 'entity' it stores.
newtype RetconDataSource entity source => ForeignKey entity source =
    ForeignKey { unForeignKey :: String }
  deriving (Eq, Ord)

instance (RetconDataSource entity source) => Show (ForeignKey entity source) where
    show = show . foreignKeyValue

-- | An existential wrapper around a 'ForeignKey'.
data SomeForeignKey = forall entity source. RetconDataSource entity source =>
    SomeForeignKey (ForeignKey entity source)

-- Aliases for clarity, all of these are value level fragments identifying
-- Internal and Foreign Keys.
type EntityName = String
type SourceName = String
type InternalID = Int
type ForeignID  = String

-- Unique, complete value level identifiers for looking up and storing Internal and
-- Foreign Keys.
type InternalKeyIdentifier = (EntityName, InternalID)
type ForeignKeyIdentifier  = (EntityName, SourceName, ForeignID)

-- | Extract the type-level information from an 'InternalKey'.
--
-- The pair contains the entity, and the key in that order.
internalKeyValue :: forall entity. (RetconEntity entity)
                 => InternalKey entity
                 -> InternalKeyIdentifier
internalKeyValue (InternalKey key) =
    let entity = symbolVal (Proxy :: Proxy entity)
    in (entity, key)

-- | Extract the type-level information from a 'ForeignKey'.
--
-- The triple contains the entity, data source, and key in that order.
foreignKeyValue :: forall entity source. (RetconDataSource entity source)
                => ForeignKey entity source
                -> ForeignKeyIdentifier
foreignKeyValue (ForeignKey key) =
    let entity = symbolVal (Proxy :: Proxy entity)
        source = symbolVal (Proxy :: Proxy source)
    in (entity, source, key)

-- | Encode a 'ForeignKey' as an opaque 'String'.
--
-- Under the hood this is represented as a showed 'ForeignKeyIdentifier'
encodeForeignKey :: forall entity source. (RetconDataSource entity source)
                 => ForeignKey entity source
                 -> String
encodeForeignKey = show . foreignKeyValue

-- | Construct a 'SomeForeignKey' value, if possible, with the supplied entity,
-- source, and key strings.
someForeignKey
    :: [SomeEntity]
    -> ForeignKeyIdentifier
    -> Maybe SomeForeignKey
someForeignKey entities (entity_name, source_name, key) =
    let entity_sym = someSymbolVal entity_name
        source_sym = someSymbolVal source_name
        same = concatMap (expandEntity entity_sym source_sym) entities
    in listToMaybe $ same
  where
    expandEntity
        :: SomeSymbol
        -> SomeSymbol
        -> SomeEntity
        -> [SomeForeignKey]
    expandEntity entity_sym source_sym (SomeEntity entity) =
        let sources = entitySources entity
            keys = mapMaybe (matching entity_sym source_sym) sources
        in keys
    matching
        :: forall entity.
           SomeSymbol
        -> SomeSymbol
        -> SomeDataSource entity
        -> Maybe SomeForeignKey
    matching (SomeSymbol entity_sym) (SomeSymbol source_sym) (SomeDataSource (source :: Proxy source)) =
        case sameSymbol entity_sym (Proxy :: Proxy entity) of
            Nothing -> Nothing
            Just _  -> case sameSymbol source_sym source of
                Nothing -> Nothing
                Just _  -> let
                    fk = ForeignKey key :: ForeignKey entity source
                    sfk = SomeForeignKey fk
                    in Just sfk

-- | Contstruct a 'SomeInternalKey' value, if possible, given the name and ID
-- values.
someInternalKey
    :: [SomeEntity]
    -> InternalKeyIdentifier
    -> Maybe SomeInternalKey
someInternalKey entities (name, key) =
    let symb = someSymbolVal name
        same = mapMaybe (matching symb) entities
    in listToMaybe same
  where
    matching
        :: SomeSymbol
        -> SomeEntity
        -> Maybe SomeInternalKey
    matching (SomeSymbol symb) (SomeEntity (entity :: Proxy entity)) =
        case sameSymbol symb entity of
            Just refl ->
                let ik = InternalKey key :: (InternalKey entity)
                    sik = SomeInternalKey ik
                in Just sik
            _         -> Nothing

-- | A storage backend for retcon operational data
--
-- In production this will interact with a PostgreSQL database, but testing and
-- demonstrations will likely use an in-memory or other low-dependency
-- alternative.
--
-- All operations must be implemented.
class RetconStore s where

    -- | Initialise a handle to the storage backend.
    --
    -- (E.g. connect to the database server, etc.)
    storeInitialise :: RetconOptions
                    -> IO s

    -- | Finalise a handle to the storage backend.
    --
    -- (E.g. disconnect from the database server, etc.)
    storeFinalise :: s
                  -> IO ()

    -- | Clone a store handle for concurrent operations. The new handle must be
    -- safe to use concurrently with the original handle.
    --
    -- (E.g. open a second connection to the same PostgreSQL database.)
    --
    -- This may be a no-op for backends which are already thread-safe (e.g.
    -- in-memory IORefs).
    storeClone
        :: s
        -> IO s

    -- | Allocate and return a new 'InternalKey'.
    storeCreateInternalKey :: forall entity. (RetconEntity entity)
                           => s
                           -> IO (InternalKey entity)

    -- | Find the 'InternalKey' associated with a 'ForeignKey'.
    storeLookupInternalKey :: (RetconDataSource e d)
                           => s
                           -> ForeignKey e d
                           -> IO (Maybe (InternalKey e))

    -- | Delete an 'InternalKey' and any associated resources.
    storeDeleteInternalKey :: (RetconEntity entity)
                           => s
                           -> InternalKey entity
                           -> IO Int

    -- | Record a 'ForeignKey' and it's association with an 'InternalKey'.
    storeRecordForeignKey :: (RetconDataSource e d)
                          => s
                          -> InternalKey e
                          -> ForeignKey e d
                          -> IO ()

    -- | Find the 'ForeignKey' corresponding to an 'InternalKey' in a particular
    -- data source.
    storeLookupForeignKey :: (RetconDataSource e d)
                          => s
                          -> InternalKey e
                          -> IO (Maybe (ForeignKey e d))

    -- | Delete a 'ForeignKey'.
    storeDeleteForeignKey :: (RetconDataSource e d)
                          => s
                          -> ForeignKey e d
                          -> IO Int

    -- | Delete all 'ForeignKey's associated with an 'InternalKey'.
    storeDeleteForeignKeys :: (RetconEntity e)
                           => s
                           -> InternalKey e
                           -> IO Int

    -- | Record the initial 'Document' associated with an 'InternalKey'.
    storeRecordInitialDocument :: (RetconEntity e)
                               => s
                               -> InternalKey e
                               -> Document
                               -> IO ()

    -- | Lookup the initial 'Document', if any, associated with an 'InternalKey'.
    storeLookupInitialDocument :: (RetconEntity e)
                               => s
                               -> InternalKey e
                               -> IO (Maybe Document)

    -- | Delete the initial 'Document', if any, associated with an 'InternalKey'.
    storeDeleteInitialDocument :: (RetconEntity e)
                               => s
                               -> InternalKey e
                               -> IO Int

    -- | Record the success 'Diff' and a list of failed 'Diff's associated with a
    -- processed 'InternalKey'.
    --
    -- Returns the ID of the recorded 'Diff's.
    storeRecordDiffs :: (RetconEntity e)
                     => s
                     -> InternalKey e
                     -> (Diff l, [Diff l])
                     -> IO Int

    -- | Record that the conflicts in a 'Diff' are resolved.
    storeResolveDiff
        :: s
        -> Int
        -> IO ()

    -- | Lookup the list of 'Diff' IDs associated with an 'InternalKey'.
    storeLookupDiffIds
        :: (RetconEntity e)
        => s
        -> InternalKey e
        -> IO [Int]

    -- | Lookup the list of conflicted 'Diff's with related information.
    --
    -- This returns serialised JSON values straight from the data store,
    -- avoiding the text -> JSON -> value -> JSON -> text overhead.
    storeLookupConflicts
        :: s
        -> IO [(ByteString, ByteString, Int, [(Int, ByteString)])]

    -- | Lookup the merged and conflicting 'Diff's with a given ID.
    storeLookupDiff
        :: s
        -> Int -- 'Diff' ID.
        -> IO (Maybe ((String, Int), Diff (), [Diff ()]))

    -- | Lookup the specified 'DiffOp's from the data store.
    --
    -- Returns triples of the 'Diff' identifier, the 'DiffOp' identifier, and
    -- the 'DiffOp'.
    storeLookupDiffConflicts
        :: s
        -> [Int]
        -> IO [(Int, Int, DiffOp ())]

    -- | Delete the 'Diff', if any, with a given ID.
    storeDeleteDiff
        :: s
        -> Int -- ^ 'Diff' ID.
        -> IO Int

    -- | Delete the 'Diff's associated with an 'InternalKey'.
    --
    -- Returns the number of 'Diff's deleted.
    storeDeleteDiffs :: (RetconEntity e)
                     => s
                     -> InternalKey e
                     -> IO Int

    -- | Record a 'Notification' associated with a given 'InternalKey'
    -- and 'Diff' ID.
    storeRecordNotification
        :: (RetconEntity e)
        => s
        -> InternalKey e
        -> Int -- ^ 'Diff' ID
        -> IO ()

    -- | Fetch and delete 'Notification's from the store.
    --
    -- Returns the number of remaining 'Notification's in the store and a list of
    -- 'Notification's for processing.
    storeFetchNotifications
        :: s
        -> Int -- ^ Maximum number to return.
        -> IO (Int, [Notification])

    -- | Add a work item to the work queue.
    storeAddWork
        :: s
        -> WorkItem
        -> IO ()

    -- | Get a work item from the work queue.
    --
    -- The item will be locked for a period of time, after which it will become
    -- available for other workers to claim.
    --
    -- TODO: The period of time is currently hardcoded in the implementations.
    storeGetWork
        :: s
        -> IO (Maybe (WorkItemID, WorkItem))

    -- | Remove a completed work item from the queue.
    storeCompleteWork
        :: s
        -> WorkItemID
        -> IO ()

-- | The identifier of a work item in the work queue.
type WorkItemID = Int

-- | An item of work to be stored in the work queue.
data WorkItem
    -- | A document was changed; process the update.
    = WorkNotify ForeignKeyIdentifier
    -- | A patch was submitted by a human; apply it.
    | WorkApplyPatch Int (Diff ())
    deriving (Show, Eq)

instance ToJSON WorkItem where
    toJSON (WorkNotify fki) = object ["notify" A..= fki]
    toJSON (WorkApplyPatch did new_diff) =
        object ["did" A..= did, "diff" A..= new_diff]

instance FromJSON WorkItem where
    parseJSON (Object v) =
        (WorkNotify <$> v .: "notify") <>
        (WorkApplyPatch <$> v .: "did" <*> v .: "diff")
    parseJSON _ = mzero

-- * Tokens

-- $ Tokens wrap storage backend values and expose particular subsets of the
-- complete storage interface to client code.

-- | Wrap a storage backend value in a token.
token :: (RetconStore s)
      => s
      -> RWToken
token = RWToken

-- | Storage tokens expose an APIs to the underlying storage backend.
--
-- Each token will carry instances of one or more other typeclasses
-- ('ReadableToken', 'WritableToken') which define the operations permitted by
-- that token type.
class StoreToken s where
    -- | Restrict a token to be read-only.
    restrictToken :: s -> ROToken

    -- | Clone a token.
    --
    -- The new token is safe to use concurrently with the existing token. See
    -- the document 'storeClone' for more details.
    cloneToken :: s -> IO s

    -- | Close the underlying store token.
    --
    -- This token must no longer be used.
    closeToken :: s -> IO ()

-- | Storage tokens which support reading operations.
class StoreToken s => ReadableToken s where
    -- | Find the 'InternalKey' associated with a 'ForeignKey'.
    lookupInternalKey :: (RetconDataSource entity d)
                      => ForeignKey entity d
                      -> RetconMonad e s l (Maybe (InternalKey entity))

    -- | Find the 'ForeignKey' corresponding to an 'InternalKey' in a particular
    -- data source.
    lookupForeignKey :: (RetconDataSource entity d)
                     => InternalKey entity
                     -> RetconMonad e s l (Maybe (ForeignKey entity d))

    -- | Lookup the initial 'Document', if any, associated with an 'InternalKey'.
    lookupInitialDocument :: (RetconEntity entity)
                          => InternalKey entity
                          -> RetconMonad e s l (Maybe Document)

    -- | Lookup IDs of 'Diff's related to a 'InternalKey'.
    lookupDiffIds
        :: (RetconEntity entity)
        => InternalKey entity
        -> RetconMonad e s l [Int]

    -- | Lookup the details of a conflicting 'Diff'.
    lookupConflicts
        :: RetconMonad e s l [(ByteString, ByteString, Int, [(Int, ByteString)])]

    -- | Lookup a 'Diff' by ID.
    lookupDiff
        :: Int
        -> RetconMonad InitialisedEntity s l (Maybe (SomeInternalKey, Diff (), [Diff ()]))

    lookupDiffConflicts
        :: [Int]
        -> RetconMonad e s l [(Int, Int, DiffOp ())]

-- | Storage tokens which support writing operations.
class ReadableToken s => WritableToken s where
    -- | Allocate and return a new 'InternalKey'.
    createInternalKey :: (RetconEntity entity)
                      => RetconMonad e s l (InternalKey entity)

    -- | Delete an 'InternalKey' and any associated resources.
    deleteInternalKey :: (RetconEntity entity)
                      => InternalKey entity
                      -> RetconMonad e s l Int

    -- | Record a 'ForeignKey' and it's association with an 'InternalKey'.
    recordForeignKey :: (RetconDataSource entity d)
                     => InternalKey entity
                     -> ForeignKey entity d
                     -> RetconMonad e s l ()

    -- | Delete a 'ForeignKey'.
    deleteForeignKey :: (RetconDataSource entity d)
                     => ForeignKey entity d
                     -> RetconMonad e s l Int

    -- | Delete all 'ForeignKey's associated with an 'InternalKey'.
    deleteForeignKeys :: (RetconEntity entity)
                      => InternalKey entity
                      -> RetconMonad e s l Int

    -- | Record the initial 'Document' associated with an 'InternalKey'.
    recordInitialDocument :: (RetconEntity entity)
                          => InternalKey entity
                          -> Document
                          -> RetconMonad e s l ()

    -- | Delete the initial 'Document', if any, associated with an 'InternalKey'.
    deleteInitialDocument :: (RetconEntity entity)
                          => InternalKey entity
                          -> RetconMonad e s l Int

    -- | Record the success 'Diff' and a list of failed 'Diff's associated with a
    -- processed 'InternalKey'.
    recordDiffs :: (RetconEntity entity)
                => InternalKey entity
                -> (Diff l, [Diff l])
                -> RetconMonad e s l Int

    -- | Delete the 'Diff' with an ID.
    deleteDiff
        :: Int
        -> RetconMonad e s l Int

    -- | Delete the 'Diff's associated with an 'InternalKey'.
    deleteDiffs :: (RetconEntity entity)
                => InternalKey entity
                -> RetconMonad e s l Int

    -- | Mark a 'Diff' as resolved.
    resolveDiff
        :: Int
        -> RetconMonad e s l ()

    -- | Record a 'Notification' associated with a given 'InternalKey'
    -- and 'Diff' ID.
    recordNotification
        :: (RetconEntity entity)
        => InternalKey entity
        -> Int
        -> RetconMonad e s l ()

    -- | Fetch and delete up to @n@ 'Notifications from the data store.
    --
    -- Returns the number of 'Notification's remaining in the data store along
    -- with the list of 'Notification'.
    fetchNotifications
        :: Int -- ^ Maximum number to fetch.
        -> RetconMonad e s l (Int, [Notification])

    -- | Add a work item to the work queue in the data store.
    addWork
        :: WorkItem
        -> RetconMonad e s l ()

    -- | Take a work item from the work queue and process it.
    processWork
        :: (WorkItem -> RetconMonad e s l v)
        -> RetconMonad e s l (Maybe v)

-- | A token exposing only the 'ReadableToken' API.
data ROToken = forall s. RetconStore s => ROToken s

instance StoreToken ROToken where
    restrictToken = id

    cloneToken (ROToken s) = ROToken <$> storeClone s

    closeToken (ROToken s) = storeFinalise s

instance ReadableToken ROToken where
    lookupInternalKey fk = do
        ROToken store <- getRetconStore
        liftIO $ storeLookupInternalKey store fk

    lookupForeignKey ik = do
        ROToken store <- getRetconStore
        liftIO $ storeLookupForeignKey store ik

    lookupInitialDocument ik = do
        ROToken store <- getRetconStore
        liftIO $ storeLookupInitialDocument store ik

    lookupConflicts = do
        ROToken store <- getRetconStore
        liftIO $ storeLookupConflicts store

    lookupDiff did = do
        ROToken store <- getRetconStore
        result <- liftIO $ storeLookupDiff store did
        massageLookupDiff did result

    lookupDiffConflicts op_ids = do
        ROToken store <- getRetconStore
        liftIO $ storeLookupDiffConflicts store op_ids

    lookupDiffIds ik = do
        ROToken store <- getRetconStore
        liftIO $ storeLookupDiffIds store ik

-- | A token exposing both the 'ReadableToken' and 'WritableToken' APIs.
data RWToken = forall s. RetconStore s => RWToken s

instance StoreToken RWToken where
    restrictToken (RWToken st) = ROToken st

    cloneToken (RWToken s) = RWToken <$> storeClone s

    closeToken (RWToken s) = storeFinalise s


instance ReadableToken RWToken where
    lookupInternalKey fk = do
        RWToken store <- getRetconStore
        liftIO $ storeLookupInternalKey store fk

    lookupForeignKey ik = do
        RWToken store <- getRetconStore
        liftIO $ storeLookupForeignKey store ik

    lookupInitialDocument ik = do
        RWToken store <- getRetconStore
        liftIO $ storeLookupInitialDocument store ik

    lookupConflicts = do
        RWToken store <- getRetconStore
        liftIO $ storeLookupConflicts store

    lookupDiff did = do
        RWToken store <- getRetconStore
        result <- liftIO $ storeLookupDiff store did
        massageLookupDiff did result

    lookupDiffConflicts op_ids = do
        RWToken store <- getRetconStore
        liftIO $ storeLookupDiffConflicts store op_ids

    lookupDiffIds ik = do
        RWToken store <- getRetconStore
        liftIO $ storeLookupDiffIds store ik

instance WritableToken RWToken where
    createInternalKey = do
        RWToken store <- getRetconStore
        liftIO $ storeCreateInternalKey store

    deleteInternalKey ik = do
        RWToken store <- getRetconStore
        liftIO $ storeDeleteInternalKey store ik

    recordForeignKey ik fk = do
        RWToken store <- getRetconStore
        liftIO $ storeRecordForeignKey store ik fk

    deleteForeignKey fk = do
        RWToken store <- getRetconStore
        liftIO $ storeDeleteForeignKey store fk

    deleteForeignKeys ik = do
        RWToken store <- getRetconStore
        liftIO $ storeDeleteForeignKeys store ik

    recordInitialDocument ik doc = do
        RWToken store <- getRetconStore
        liftIO $ storeRecordInitialDocument store ik doc

    deleteInitialDocument ik = do
        RWToken store <- getRetconStore
        liftIO $ storeDeleteInitialDocument store ik

    recordDiffs ik diffs = do
        RWToken store <- getRetconStore
        liftIO $ storeRecordDiffs store ik diffs

    deleteDiff did = do
        RWToken store <- getRetconStore
        liftIO $ storeDeleteDiff store did

    deleteDiffs ik = do
        RWToken store <- getRetconStore
        liftIO $ storeDeleteDiffs store ik

    resolveDiff did = do
        RWToken store <- getRetconStore
        liftIO $ storeResolveDiff store did

    recordNotification ik did = do
        RWToken store <- getRetconStore
        liftIO $ storeRecordNotification store ik did

    fetchNotifications limit = do
        RWToken store <- getRetconStore
        liftIO $ storeFetchNotifications store limit

    addWork work = do
        RWToken store <- getRetconStore
        liftIO $ storeAddWork store work

    processWork worker = do
        RWToken store <- getRetconStore
        LE.handle
            (\e -> logException e >> return Nothing)
            (do
                work <- liftIO $ storeGetWork store
                case work of
                    Nothing -> return Nothing
                    Just (work_id, item) -> do
                        result <- worker item
                        liftIO $ storeCompleteWork store work_id
                        return $ Just result
            )
      where
        logException :: SomeException -> RetconMonad e s l ()
        logException e =
            case fromException e :: Maybe AsyncException of
                Just e' -> LE.throw e'
                Nothing -> logErrorN . fromString $ "Error processing work: " <> show e

-- | Massage the result of 'lookupDiff' to include a SomeInternalKey rather
-- than (String, Int).
--
-- This is used in the implementations of 'lookupDiff' above.
massageLookupDiff
    :: Int -- ^ Diff ID.
    -> Maybe (InternalKeyIdentifier, Diff (), [Diff ()])
    -> RetconMonad InitialisedEntity s l
       (Maybe (SomeInternalKey, Diff (), [Diff ()]))
massageLookupDiff did result = do
    entities <- map dropEntityState <$> getRetconState
    case result of
        Nothing -> return Nothing
        Just (ik', dif, conf) ->
            case someInternalKey entities ik' of
                Just sk -> return $ Just (sk, dif, conf)
                Nothing -> do
                    logErrorN . fromString $
                        "Unable to construct InternalKey for diff: "
                        <> show did
                    return Nothing
