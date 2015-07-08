{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Retcon.Store.Memory
     ( MemStore
     , Mem
     ) where

import           Control.Applicative
import           Control.Lens
import qualified Data.Aeson.Diff     as D
import           Data.IORef
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Monoid

import           Retcon.Diff
import           Retcon.Document
import           Retcon.Identifier
import           Retcon.Store.Base

-- | An acid-state like in-memory store.
type Mem = IORef MemStore

data MemStore = MemStore
    { _memNextKey :: Int
    , _memItoF    :: Map InternalKey (Map SourceName ForeignID)
    , _memFtoI    :: Map ForeignKey InternalID
    , _memInits   :: Map InternalKey Document
    , _memDiffs   :: Map InternalKey [(D.Patch, [D.Operation])]
    }
makeLenses ''MemStore

emptyMem :: MemStore
emptyMem = MemStore 0 mempty mempty mempty mempty

-- | "Open the module" with the in-memory store type.
--
instance Store (IORef MemStore) where
  newtype StoreOpts       (IORef MemStore)   = MemOpts ()

  initBackend (MemOpts{}) = newIORef emptyMem

  closeBackend  = flip writeIORef emptyMem

  cloneStore    = return

  createInternalKey ref entity =
    atomicModifyIORef' ref $ \st ->
      (memNextKey +~ 1 $ st, InternalKey entity (st ^. memNextKey))

  lookupInternalKey ref fk = do
    st <- readIORef ref
    return $ st ^? memFtoI . ix fk . to (InternalKey (fkEntity fk))

  deleteInternalKey ref ik =
    atomicModifyIORef' ref $ \st ->
        (st & memItoF . at ik .~ Nothing, 0)

  recordForeignKey ref ik fk =
    let source_name = fkSource fk
        foreign_id  = fkID     fk
        internal_id = ikID     ik
    in  atomicModifyIORef' ref $ \st ->
          ( st & memItoF . at ik . non mempty . at source_name ?~ foreign_id
               & memFtoI . at fk ?~ internal_id
          , ())

  lookupForeignKey ref source_name ik = do
      let entity_name = ikEntity ik
      st <- readIORef ref
      -- Construct a new ForeignKey if it exists in the memItoF map
      return $ st ^? memItoF . ix ik . ix source_name . to (ForeignKey entity_name source_name)

  deleteForeignKey ref fk = do
    maybe_ik <- lookupInternalKey ref fk
    atomicModifyIORef' ref $ \st ->
        -- Go through the internal to foreign map removing all foreign ids
        -- that match this value under the associated internal key.
        ( st & maybe id (\iki -> memItoF . ix iki %~ M.filter (/= fkID fk)) maybe_ik
        -- Also, delete the foreign in the foreign to internal map.
             & memFtoI . at fk .~ Nothing
        , 0)

  deleteForeignKeysWithInternal ref ik = do
      let entity_name = ikEntity ik
      atomicModifyIORef' ref $ \st ->
          -- List of the foreign key identifiers associated with the internal
          -- key
          let fkis = fmap (uncurry (ForeignKey entity_name)) $
                      st ^.. memItoF . at ik . _Just . itraversed . withIndex in
          -- Now delete the foreign keys from the foreign to internal map
          ( st & memFtoI %~ (\ftoi -> foldr M.delete ftoi fkis)
          , ())
      -- Now remove the internal key.
      deleteInternalKey ref ik

  recordInitialDocument ref ik doc =
      atomicModifyIORef' ref $ \st ->
          (st & memInits . at ik ?~ doc, ())

  lookupInitialDocument ref ik =
      (^. memInits . at ik) <$> readIORef ref

  deleteInitialDocument ref ik =
    atomicModifyIORef' ref $ \st ->
      (st & memInits . at ik .~ Nothing, 0)

  recordDiffs ref ik new =
    let new' = bimap (^. patchDiff) (^.. traverse . rejectedOperation) new
    in atomicModifyIORef' ref $ \st ->
      -- Slow due to list traversal
      (st & memDiffs . at ik . non mempty <%~ (++[new']))
      ^. swapped & _2 %~ length

  -- TODO Implement
  lookupDiff ref _ =
    let get st = (st, Nothing)
    in  atomicModifyIORef' ref get

  -- TODO Implement
  lookupDiffIDs ref _ =
    let get st = (st, [])
    in  atomicModifyIORef' ref get

  -- TODO: Implement
  deleteDiff ref _ =
    let del st = (st, 0)
    in  atomicModifyIORef' ref del

  deleteDiffsWithKey ref ik =
      atomicModifyIORef' ref $ \st ->
          (st & memDiffs . at ik .~ Nothing, 0)

  resolveDiffs = const . const $ return ()

  reduceDiff = const . const . const $ return ()

  lookupConflicts = const $ return []
  lookupDiffConflicts = const . const $ return []

  addWork      = const . const $ return ()
  getWork      = const $ return Nothing
  ungetWork    = const . const $ return ()
  completeWork = const . const $ return ()
