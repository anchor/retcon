--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description: Dispatch events with a retcon configuration.

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Retcon.Handler where

import Control.Applicative
import Control.Exception.Enclosed (tryAny)
import Control.Monad.Error.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Bifunctor
import Data.Either
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.String
import Data.Type.Equality
import GHC.TypeLits

import Retcon.Core
import Retcon.Diff
import Retcon.Document
import Retcon.Error
import Retcon.MergePolicy
import Retcon.Monad
import Retcon.Options

-- * Retcon

-- $ An invocation of the retcon system will recieve and process a single event from
-- the outside world. It does this by first determining the type of operation to be
-- performed and then executing that command.

-- | Run the retcon process on an event.
retcon
    :: (ReadableToken s, WritableToken s)
    => RetconConfig SomeEntity s
    -> String
    -> IO (Either RetconError ())
retcon config key =
    runRetconMonadOnce config () . dispatch $ read key

-- | Parse a request string and handle an event.
dispatch
    :: forall store. (ReadableToken store, WritableToken store)
    => (String, String, String)
    -> RetconHandler store ()
dispatch (entity_str, source_str, key) = do
    entities <- getRetconState

    case (someSymbolVal entity_str, someSymbolVal source_str) of
        (SomeSymbol entity, SomeSymbol source) -> do
            res <- anyM entities $ \(InitialisedEntity e dss) ->
                if same e entity
                  then do
                      res <- anyM dss $ \(InitialisedSource (sp :: Proxy st) dst :: InitialisedSource et) -> do
                          let fk = ForeignKey key :: ForeignKey et st

                          if same source sp
                            then process dst fk >> return True
                            else return False
                      if res
                        then return True
                        else do
                            $logError . fromString $
                                "Cannot process unknown data source: " <>
                                show (entity_str, source_str, key)
                            return False
                   else return False
            unless res $ $logError . fromString $
                "Cannot process unknown entity: " <>
                show (entity_str, source_str, key)
  where
    anyM :: Monad m => [a] -> (a -> m Bool) -> m Bool
    anyM xs f = foldM (\b x -> if b then return True else liftM (|| b) (f x)) False xs

-- * Operations

-- $ The operations performed by retcon are described, at a high level, by
-- 'RetconOperation's.

-- | Operations to be performed in response to data source events.

-- TODO: Add fk, ik, and doc to constructors so that we don't need to re-query
-- them when executing the operation.
data RetconOperation entity source
    = RetconCreate (ForeignKey entity source) -- ^ Create a new document.
    | RetconDelete (InternalKey entity)       -- ^ Delete an existing document.
    | RetconUpdate (InternalKey entity)       -- ^ Update an existing document.
    | RetconProblem (ForeignKey entity source) RetconError -- ^ Record an error.
  deriving (Show)

instance Eq (RetconOperation entity source) where
    (RetconCreate fk1) == (RetconCreate fk2) = fk1 == fk2
    (RetconDelete ik1) == (RetconDelete ik2) = ik1 == ik2
    (RetconUpdate ik1) == (RetconUpdate ik2) = ik1 == ik2
    (RetconProblem fk1 _) == (RetconProblem fk2 _) = fk1 == fk2
    _ == _ = False

-- | Process an event on a specified 'ForeignKey'.
--
-- This function is responsible for determining the type of event which has
-- occured and invoking the correct 'RetconDataSource' actions and retcon
-- algorithms to handle it.
process
    :: forall store entity source.
       (ReadableToken store, WritableToken store, RetconDataSource entity source)
    => DataSourceState entity source
    -> ForeignKey entity source
    -> RetconHandler store ()
process state fk = do
    whenVerbose . $logDebug . fromString $
        "EVENT against " <> (show . length $ sources) <> " sources."

    determineOperation state fk >>= runOperation state
  where
    sources = entitySources (Proxy :: Proxy entity)

-- | Construct a 'RetconOperation' to be performed by this invocation of
-- retcon.
--
-- The 'RetconOperation' is determined based on the presence and absence of an
-- 'InternalKey' and 'Document' corresponding to the 'ForeignKey' which triggered
-- the invocation.
determineOperation
    :: (ReadableToken s, RetconDataSource entity source)
    => DataSourceState entity source
    -> ForeignKey entity source
    -> RetconHandler s (RetconOperation entity source)
determineOperation state fk = do
    whenVerbose . $logDebug . fromString $
        "DETERMINE: " <> show fk

    -- Lookup the corresponding InternalKey.
    ik' <- lookupInternalKey fk

    whenVerbose . $logDebug . fromString $
        "Looking for internal key for: " <> show fk <> "; found " <> show ik'

    -- Fetch the corresponding Document.
    doc' <- runRetconAction state $ getDocument fk

    whenVerbose . $logDebug . fromString $
        "Looking for document for: " <> show fk <> "; found " <> show doc'

    -- Determine the RetconOperation to be performed.
    let operation = case (ik', doc') of
            (Nothing, Left  _) -> RetconProblem fk (RetconSourceError "Unknown key, no document")
            (Nothing, Right _) -> RetconCreate fk
            (Just ik, Left  _) -> RetconDelete ik
            (Just ik, Right _) -> RetconUpdate ik

    whenVerbose . logInfoN . fromString $
        "DETERMINED: " <> show fk <> " operation: " <> show operation

    return operation

-- | Execute the action described by a 'RetconOperation' value.
runOperation
    :: (ReadableToken store, WritableToken store, RetconDataSource entity source)
    => DataSourceState entity source
    -> RetconOperation entity source
    -> RetconHandler store ()
runOperation state event =
    case event of
        RetconCreate  fk -> create state fk
        RetconDelete  ik -> delete ik
        RetconUpdate  ik -> update ik
        RetconProblem fk err -> reportError fk err

-- ** Execute operations

-- $ These function execute the operations represented by 'RetconOperation' values.

-- | Execute a 'RetconCreate' operation.
create
    :: forall store entity source.
       (ReadableToken store, WritableToken store, RetconDataSource entity source)
    => DataSourceState entity source
    -> ForeignKey entity source
    -> RetconHandler store ()
create state fk = do
    logInfoN . fromString $
        "CREATE: " <> show fk

    -- Allocate a new InternalKey to represent this entity.
    ik <- createInternalKey
    recordForeignKey ik fk

    -- Use the new Document as the initial document.
    doc' <- runRetconAction state $ getDocument fk

    results <- case doc' of
        Left _ -> do
            deleteState ik
            throwError (RetconSourceError "Cannot create document which doesn't exist")
        Right doc -> do
            recordInitialDocument ik doc
            -- TODO: This should probably be using the InitialisedEntity list?
            setDocuments ik . map (const doc) $ entitySources (Proxy :: Proxy entity)

    -- Record any errors in the log.
    let (failed, _success) = partitionEithers results
    unless (null failed) $
        $logError . fromString $
            "ERROR creating " <> show ik <> " from " <> show fk <> ". " <>
            show failed

    return ()

-- | Execute a 'RetconDelete' event.
delete
    :: (ReadableToken store, WritableToken store, RetconEntity entity)
    => InternalKey entity
    -> RetconHandler store ()
delete ik = do
    logInfoN . fromString $
        "DELETE: " <> show ik

    -- Delete from data sources.
    results <- deleteDocuments ik

    -- Record failures in the log.
    let (_succeeded, failed) = partitionEithers results
    unless (null failed) $
        $logError . fromString $
            "ERROR deleting " <> show ik <> ". " <> show failed

    -- Delete the internal associated with the key.
    deleteState ik

-- | Process an update event.
update
    :: (ReadableToken store, WritableToken store, RetconEntity entity)
    => InternalKey entity
    -> RetconHandler store ()
update ik = do
    logInfoN . fromString $
        "UPDATE: " <> show ik

    -- Fetch documents, logging any errors.
    docs <- getDocuments ik
    let (failures, valid) = partitionEithers docs
    unless (null failures) $
        $logWarn . fromString $
            "WARNING updating " <> show ik <> ". Unable to fetch some documents: "
            <> show failures

    -- Find or calculate the initial document.
    --
    -- TODO: This is fragile in the case that only one data sources has a document.
    initial <- fromMaybe (calculateInitialDocument valid) <$>
               lookupInitialDocument ik

    -- Build the diff from non-missing documents.
    let diffs = map (diff initial) valid
    let (merged, fragments) = mergeDiffs ignoreConflicts diffs

    whenVerbose . $logInfo . fromString $
        "Conflict detected merging: " <> show ik

    -- Replace any missing 'Document's with the intial document.
    let docs' = map (either (const initial) id) docs

    -- Apply the 'Diff' to the 'Document's, and save everything.
    distributeDiff ik initial (void merged, map void fragments) docs'

    return ()

-- | Apply a 'Diff' to the 'Document's associated with an 'InternalKey'.
--
-- This is the second part to 'update' and is used by the conflict resolution
-- API calls.
--
-- TODO: Rename this function. Also implement it. Mostly this means moving code
-- from 'update' into this function.
distributeDiff
    :: (ReadableToken store, WritableToken store, RetconEntity entity)
    => InternalKey entity
    -> Document
    -> (Diff (), [Diff ()])
    -> [Document]
    -> RetconHandler store ()
distributeDiff ik initial (merged, fragments) docs = do

    -- Apply the diff to each source document.
    --
    -- We replace documents we couldn't get with the initial document. The
    -- initial document may not be "valid". These missing cases are logged
    -- above.
    let output = map (applyDiff merged) docs

    -- Save documents, logging any errors.
    results <- setDocuments ik output
    let (failed, _) = partitionEithers results
    unless (null failed) $
        $logWarn . fromString $
            "WARNING updating " <> show ik <> ". Unable to set some documents: "
            <> show failed

    -- Record changes in database.
    did <- recordDiffs ik (merged, fragments)

    -- Record notifications, if required.
    unless (null fragments) $
        recordNotification ik did

    -- Update the initial document.
    let initial' = applyDiff merged initial
    recordInitialDocument ik initial'

    return ()

-- | Report an error in determining the operation, communicating with the data
-- source or similar.
reportError
    :: (RetconDataSource entity source)
    => ForeignKey entity source
    -> RetconError
    -> RetconHandler store ()
reportError fk err = do
    logInfoN . fromString $
        "ERROR: " <> show fk

    logErrorN . fromString $
        "Could not process event for " <> show fk <> ". " <> show err

    return ()

-- * Data source wrappers

-- $ These actions wrap the operations for a single data source and apply them
-- lists of arbitrary data sources.

-- | Get 'Document's corresponding to an 'InternalKey' for all sources for an
-- entity.
getDocuments
    :: forall store entity. (ReadableToken store, RetconEntity entity)
    => InternalKey entity
    -> RetconHandler store [Either RetconError Document]
getDocuments ik = do
    let entity = Proxy :: Proxy entity
    entities <- getRetconState

    results <- forM entities $ \(InitialisedEntity current sources) ->
        case sameSymbol entity current of
            Nothing -> return []
            Just Refl -> forM sources $ \(InitialisedSource (_ :: Proxy source) state) ->
                -- Flatten any nested errors.
                (do
                    -- Lookup the foreign key for this data source.
                    mkey :: Maybe (ForeignKey entity source) <- lookupForeignKey ik
                    whenVerbose . $logDebug . fromString $
                        "Lookup of " <> show ik <> " resulted in " <> show mkey
                    -- If there was a key, use it to fetch the document.
                    case mkey of
                        Nothing -> return . Left $ RetconFailed
                        Just fk -> do
                            res <- runRetconAction state $ getDocument fk
                            whenVerbose . $logDebug . fromString $
                                "Retrieved document " <> show fk <> ": " <> show res
                            return res
                    )
    return . concat $ results

-- | Set 'Document's corresponding to an 'InternalKey' for all sources for an
-- entity.
setDocuments
    :: forall store entity.
       (ReadableToken store, WritableToken store, RetconEntity entity)
    => InternalKey entity
    -> [Document]
    -> RetconHandler store [Either RetconError ()]
setDocuments ik docs = do
    let entity = Proxy :: Proxy entity
    entities <- getRetconState

    results <- forM entities $ \(InitialisedEntity current sources) ->
        case sameSymbol entity current of
            Nothing -> return []
            Just Refl -> forM (zip docs sources) $
                \(doc, InitialisedSource (_ :: Proxy source) state) ->
                    join . first RetconError <$> tryAny (do
                        (fk :: Maybe (ForeignKey entity source)) <- lookupForeignKey ik
                        fk' <- runRetconAction state $ setDocument doc fk
                        case fk' of
                            Left  _  -> return ()
                            Right newfk -> void $ recordForeignKey ik newfk
                        return $ Right ()
                    )
    return . concat $ results

-- | Delete the 'Document' corresponding to an 'InternalKey' for all sources.
deleteDocuments
    :: forall store entity.
       (ReadableToken store, WritableToken store, RetconEntity entity)
    => InternalKey entity
    -> RetconHandler store [Either RetconError ()]
deleteDocuments ik = do
    let entity = Proxy :: Proxy entity
    entities <- getRetconState

    -- Iterate over the list of entities.
    results <- forM entities $ \(InitialisedEntity current sources) ->
        -- When you've found the one corresponding to the 'InternalKey'.
        case sameSymbol entity current of
            Nothing -> return []
            Just Refl ->
                -- Iterate of the associated sources.
                forM sources $ \(InitialisedSource (_:: Proxy source) state) -> do
                    -- Map the InternalKey to a ForeignKey for this source and...
                    (fk' :: Maybe (ForeignKey entity source)) <- lookupForeignKey ik
                    -- ...issue a delete.
                    case fk' of
                        Nothing -> return . Right $ ()
                        Just fk -> runRetconAction state $ deleteDocument fk
    return . concat $ results

-- * Storage backend wrappers

-- $ These actions wrap the data storage backend operations and lift them into
-- the RetconHandler monad.

-- | Delete the internal state associated with an 'InternalKey'.
deleteState
    :: forall store entity. (WritableToken store, RetconEntity entity)
    => InternalKey entity
    -> RetconHandler store ()
deleteState ik = do
    logInfoN . fromString $
        "DELETE: " <> show ik

    -- Delete the initial document.
    n_id <- deleteInitialDocument ik
    whenVerbose . logDebugN . fromString $
        "Deleted initial document for " <> show ik <> ". Deleted " <> show n_id

    -- TODO: Do we need to delete notifications here? I think we do!

    -- Delete the diffs.
    n_diff <- deleteDiffs ik
    whenVerbose . logDebugN . fromString $
        "Deleted diffs for " <> show ik <> ": " <> show n_diff

    -- Delete associated foreign keys.
    n_fk <- deleteForeignKeys ik
    whenVerbose . logDebugN . fromString $
        "Deleted foreign key/s for " <> show ik <> ": " <> show n_fk

    -- Delete the internal key.
    n_ik <- deleteInternalKey ik
    whenVerbose . logDebugN . fromString $
        "Deleted internal key/s for " <> show ik <> ": " <> show n_ik

    return ()

-- | Attempt to translate a 'ForeignKey' from one source into a 'ForeignKey'
-- for another source.
translateForeignKey
    :: forall entity source1 source2 store.
       (ReadableToken store, RetconDataSource entity source1,
       RetconDataSource entity source2)
    => ForeignKey entity source1
    -> RetconHandler store (Maybe (ForeignKey entity source2))
translateForeignKey from =
    lookupInternalKey from >>= maybe (return Nothing) lookupForeignKey

-- * Utility functions

-- | Check that two symbols are the same.
same
    :: (KnownSymbol a, KnownSymbol b)
    => Proxy a
    -> Proxy b
    -> Bool
same a b = isJust (sameSymbol a b)
