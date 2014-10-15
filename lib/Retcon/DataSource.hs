--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: API to implement entities and data sources.

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Retcon.DataSource where

import Control.Applicative
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Reader
import Data.Biapplicative
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Type.Equality
import GHC.TypeLits

import Retcon.Diff
import Retcon.Document
import Retcon.Monad
import Retcon.Notifications
import Retcon.Options
import Utility.Configuration

import Control.Lens

-- | Configuration value for retcon.
newtype RetconConfig =
    RetconConfig { retconEntities :: [SomeEntity] }

-- | Restricted monad with read-only access to the retcon storage.
type RetconAction l a = RetconMonad InitialisedEntity ROToken l a

type RetconHandler s a = RetconMonad InitialisedEntity s () a

-- * Entities

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

-- * Data sources

-- | Monad for initialisers.
newtype Initialiser s a = Initialiser {
    unInitialiser :: ReaderT s IO a
    }
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

-- * Wrapper types
--
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

-- | Wrap an arbitrary 'RetconDataSource' for some entity 'e', together with
-- it's initialised state.
data InitialisedSource e = forall s. RetconDataSource e s =>
    InitialisedSource { sourceProxy :: Proxy s
                      , sourceState :: DataSourceState e s
                      }

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
    findEntity Nothing (InitialisedEntity entityProxy entityState) =
        case sameSymbol entityProxy entity of
            Just Refl -> foldl findSource Nothing entityState
            Nothing   -> Nothing
    findEntity r       _ = r

    findSource :: Maybe (DataSourceState e d)
               -> InitialisedSource e
               -> Maybe (DataSourceState e d)
    findSource Nothing (InitialisedSource sourceProxy sourceState) =
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
            let param = maybe mempty id $ M.lookup (names) params
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
        let param = maybe mempty id $ M.lookup (names) params
        runInitialiser param $ finaliseState s
        return ds

-- * Keys
--
-- $ The various parts of retcon refer to documents using two types of key
-- values: an 'InternalKey entity' identifies a 'Document' for a whole entity
-- and a 'ForeignKey entity source' identifies a 'Document' in a particular
-- data source.

-- | The unique identifier used to identify a unique 'entity' document within
-- retcon.
newtype RetconEntity entity => InternalKey entity =
    InternalKey { unInternalKey :: Int }
  deriving (Eq, Ord, Show)

-- | Extract the type-level information from an 'InternalKey'.
--
-- The pair contains the entity, and the key in that order.
internalKeyValue :: forall entity. (RetconEntity entity)
                 => InternalKey entity
                 -> (String, Int)
internalKeyValue (InternalKey key) =
    let entity = symbolVal (Proxy :: Proxy entity)
    in (entity, key)

-- | The unique identifier used by the 'source' data source to refer to an
-- 'entity' it stores.
newtype RetconDataSource entity source => ForeignKey entity source =
    ForeignKey { unForeignKey :: String }
  deriving (Eq, Ord, Show)

-- | Extract the type-level information from a 'ForeignKey'.
--
-- The triple contains the entity, data source, and key in that order.
foreignKeyValue :: forall entity source. (RetconDataSource entity source)
                => ForeignKey entity source
                -> (String, String, String)
foreignKeyValue (ForeignKey key) =
    let entity = symbolVal (Proxy :: Proxy entity)
        source = symbolVal (Proxy :: Proxy source)
    in (entity, source, key)

-- | Encode a 'ForeignKey' as a 'String'.
encodeForeignKey :: forall entity source. (RetconDataSource entity source)
                 => ForeignKey entity source
                 -> String
encodeForeignKey = show . foreignKeyValue

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
                           -> IO ()

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
                          -> IO ()

    -- | Delete all 'ForeignKey's associated with an 'InternalKey'.
    storeDeleteForeignKeys :: (RetconEntity e)
                           => s
                           -> InternalKey e
                           -> IO ()

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
                               -> IO ()

    -- | Record the success 'Diff' and a list of failed 'Diff's associated with a
    -- processed 'InternalKey'.
    --
    -- Returns the ID of the recorded 'Diff's.
    storeRecordDiffs :: (RetconEntity e)
                     => s
                     -> InternalKey e
                     -> (Diff l, [Diff l])
                     -> IO Int

    -- | Lookup the list of 'Diff' IDs associated with an 'InternalKey'.
    storeLookupDiffIds
        :: (RetconEntity e)
        => s
        -> InternalKey e
        -> IO [Int]

    -- | Lookup the merged and conflicting 'Diff's with a given ID.
    storeLookupDiff
        :: s
        -> Int -- 'Diff' ID.
        -> IO (Maybe (Diff (), [Diff ()]))

    -- | Delete the 'Diff', if any, with a given ID.
    storeDeleteDiff
        :: s
        -> Int -- ^ 'Diff' ID.
        -> IO ()

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

    -- | Lookup a 'Diff' by ID.
    lookupDiff
        :: Int
        -> RetconMonad e s l (Maybe (Diff (), [Diff ()]))

-- | Storage tokens which support writing operations.
class StoreToken s => WritableToken s where
    -- | Allocate and return a new 'InternalKey'.
    createInternalKey :: (RetconEntity entity)
                      => RetconMonad e s l (InternalKey entity)

    -- | Delete an 'InternalKey' and any associated resources.
    deleteInternalKey :: (RetconEntity entity)
                      => InternalKey entity
                      -> RetconMonad e s l ()

    -- | Record a 'ForeignKey' and it's association with an 'InternalKey'.
    recordForeignKey :: (RetconDataSource entity d)
                     => InternalKey entity
                     -> ForeignKey entity d
                     -> RetconMonad e s l ()

    -- | Delete a 'ForeignKey'.
    deleteForeignKey :: (RetconDataSource entity d)
                     => ForeignKey entity d
                     -> RetconMonad e s l ()

    -- | Delete all 'ForeignKey's associated with an 'InternalKey'.
    deleteForeignKeys :: (RetconEntity entity)
                      => InternalKey entity
                      -> RetconMonad e s l ()

    -- | Record the initial 'Document' associated with an 'InternalKey'.
    recordInitialDocument :: (RetconEntity entity)
                          => InternalKey entity
                          -> Document
                          -> RetconMonad e s l ()

    -- | Delete the initial 'Document', if any, associated with an 'InternalKey'.
    deleteInitialDocument :: (RetconEntity entity)
                          => InternalKey entity
                          -> RetconMonad e s l ()

    -- | Record the success 'Diff' and a list of failed 'Diff's associated with a
    -- processed 'InternalKey'.
    recordDiffs :: (RetconEntity entity)
                => InternalKey entity
                -> (Diff l, [Diff l])
                -> RetconMonad e s l Int

    -- | Delete the 'Diff' with an ID.
    deleteDiff
        :: Int
        -> RetconMonad e s l ()

    -- | Delete the 'Diff's associated with an 'InternalKey'.
    deleteDiffs :: (RetconEntity entity)
                => InternalKey entity
                -> RetconMonad e s l Int

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

-- | A token exposing only the 'ReadableToken' API.
data ROToken = forall s. RetconStore s => ROToken s

instance StoreToken ROToken where
    restrictToken = id

instance ReadableToken ROToken where
    lookupInternalKey fk = do
        ROToken store <- view retconStore
        liftIO $ storeLookupInternalKey store fk

    lookupForeignKey ik = do
        ROToken store <- view retconStore
        liftIO $ storeLookupForeignKey store ik

    lookupInitialDocument ik = do
        ROToken store <- view retconStore
        liftIO $ storeLookupInitialDocument store ik

    lookupDiff did = do
        ROToken store <- view retconStore
        liftIO $ storeLookupDiff store did

    lookupDiffIds ik = do
        ROToken store <- view retconStore
        liftIO $ storeLookupDiffIds store ik

-- | A token exposing both the 'ReadableToken' and 'WritableToken' APIs.
data RWToken = forall s. RetconStore s => RWToken s

instance StoreToken RWToken where
    restrictToken (RWToken st) = ROToken st

instance ReadableToken RWToken where
    lookupInternalKey fk = do
        RWToken store <- view retconStore
        liftIO $ storeLookupInternalKey store fk

    lookupForeignKey ik = do
        RWToken store <- view retconStore
        liftIO $ storeLookupForeignKey store ik

    lookupInitialDocument ik = do
        RWToken store <- view retconStore
        liftIO $ storeLookupInitialDocument store ik

    lookupDiff did = do
        RWToken store <- view retconStore
        liftIO $ storeLookupDiff store did

    lookupDiffIds ik = do
        RWToken store <- view retconStore
        liftIO $ storeLookupDiffIds store ik

instance WritableToken RWToken where
    createInternalKey = do
        RWToken store <- view retconStore
        liftIO $ storeCreateInternalKey store

    deleteInternalKey ik = do
        RWToken store <- view retconStore
        liftIO $ storeDeleteInternalKey store ik

    recordForeignKey ik fk = do
        RWToken store <- view retconStore
        liftIO $ storeRecordForeignKey store ik fk

    deleteForeignKey fk = do
        RWToken store <- view retconStore
        liftIO $ storeDeleteForeignKey store fk

    deleteForeignKeys ik = do
        RWToken store <- view retconStore
        liftIO $ storeDeleteForeignKeys store ik

    recordInitialDocument ik doc = do
        RWToken store <- view retconStore
        liftIO $ storeRecordInitialDocument store ik doc

    deleteInitialDocument ik = do
        RWToken store <- view retconStore
        liftIO $ storeDeleteInitialDocument store ik

    recordDiffs ik diffs = do
        RWToken store <- view retconStore
        liftIO $ storeRecordDiffs store ik diffs

    deleteDiff did = do
        RWToken store <- view retconStore
        liftIO $ storeDeleteDiff store did

    deleteDiffs ik = do
        RWToken store <- view retconStore
        liftIO $ storeDeleteDiffs store ik

    recordNotification ik did = do
        RWToken store <- view retconStore
        liftIO $ storeRecordNotification store ik did

    fetchNotifications limit = do
        RWToken store <- view retconStore
        liftIO $ storeFetchNotifications store limit
