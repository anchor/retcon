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
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Maybe
import Data.Proxy
import Data.Type.Equality
import GHC.TypeLits

import Retcon.Document
import Retcon.Error

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
    initialiseState :: IO (DataSourceState entity source)

    -- | Finalise the state used by the data source.
    --
    -- This is called during a clean shutdown to, for example, cleanly close a
    -- database connection, etc.
    finaliseState :: DataSourceState entity source
                  -> IO ()

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
                -> DataSourceAction s (DataSourceState entity source) (ForeignKey entity source)

    -- | Retrieve a document from a data source.
    --
    -- If the document cannot be retrieved an error is returned in the 'Retcon'
    -- monad.
    getDocument :: ForeignKey entity source
                -> DataSourceAction s (DataSourceState entity source) Document

    -- | Delete a document from a data source.
    --
    -- If the document cannot be deleted an error is returned in the 'Retcon'
    -- monad.
    deleteDocument :: ForeignKey entity source
                   -> DataSourceAction s (DataSourceState entity source) ()

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
initialiseEntities :: [SomeEntity]
                   -> IO [InitialisedEntity]
initialiseEntities = mapM initialiseEntity
  where
    initialiseEntity :: SomeEntity -> IO InitialisedEntity
    initialiseEntity (SomeEntity (p :: Proxy e)) = do
        ss <- initialiseSources $ entitySources p
        return $ InitialisedEntity p ss

-- | Finalise the states for a collection of entities.
finaliseEntities :: [InitialisedEntity]
                 -> IO [SomeEntity]
finaliseEntities = mapM finaliseEntity . reverse
  where
    finaliseEntity (InitialisedEntity p s) = do
        _ <- finaliseSources $ reverse s
        return $ SomeEntity p

-- | Initialise the states for a collection of data sources.
initialiseSources :: forall e. RetconEntity e
                 => [SomeDataSource e]
                 -> IO [InitialisedSource e]
initialiseSources = mapM initialiseSource
  where
    initialiseSource (SomeDataSource (p :: Proxy s) :: SomeDataSource e) = do
        s <- initialiseState
        return $ InitialisedSource p s

-- | Finalise the states for a collection of data sources.
finaliseSources :: forall e. RetconEntity e
                 => [InitialisedSource e]
                 -> IO [SomeDataSource e]
finaliseSources = mapM finaliseSource
  where
    finaliseSource :: InitialisedSource e -> IO (SomeDataSource e)
    finaliseSource (InitialisedSource p s) = do
        finaliseState s
        return $ SomeDataSource p

-- * Monads

-- $ Operations which interact with external "data source" systems are
-- implemented in the 'DataSourceAction' monad.

-- | Monad transformer stack used in the 'DataSourceAction' monad.
type DataSourceActionStack store state = ReaderT state (ReaderT store (ExceptT RetconError (LoggingT IO)))

-- | Monad for interactions with data sources.
--
-- This monad provides error handling, logging, and I/O facilities.
newtype DataSourceAction store state a =
    DataSourceAction {
        unDataSourceAction :: DataSourceActionStack store state a
    }
  deriving (Functor, Applicative, Monad, MonadBase IO, MonadIO, MonadLogger,
  MonadError RetconError, MonadReader state)

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
