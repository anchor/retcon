{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Retcon.Store.Base
     ( -- * Database
       Store(..)

       -- * Work
     , WorkItem(..)
     , WorkItemID

       -- * Operation responses
     , ConflictResp(..)
     , conflictRawDoc, conflictRawDiff, conflictDiffID, conflictRawOps
     , OpResp(..)
     , opDiffID, opID, ops
     , DiffResp(..)
     , diffEntity, diffKey, diffPatch, diffConflicts

     -- * Type aliases
     , DiffID
     , OpID

     ) where

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson.Diff     as D
import           Data.ByteString     (ByteString)
import           Data.Monoid

import           Retcon.Diff
import           Retcon.Document
import           Retcon.Identifier

type DiffID  = Int
type OpID    = Int

data ConflictResp = ConflictResp
  { _conflictRawDoc  :: ByteString
  , _conflictRawDiff :: ByteString
  , _conflictDiffID  :: DiffID
  , _conflictRawOps  :: [(OpID, ByteString)]
  } deriving (Show)
makeLenses ''ConflictResp

data DiffResp = DiffResp
  { _diffEntity    :: ByteString
  , _diffKey       :: Int
  , _diffPatch     :: D.Patch
  , _diffConflicts :: [D.Operation]
  } deriving (Show)
makeLenses ''DiffResp

data OpResp = OpResp
  { _opDiffID :: DiffID
  , _opID     :: OpID
  , _ops      :: D.Operation
  } deriving (Show)
makeLenses ''OpResp

-- | The internal store "module".
--
class Store store where
  data StoreOpts opts

  -- | Initialise a handle to the storage backend.
  initBackend  :: StoreOpts store -> IO store

  -- | Finalise a handle to the storage backend.
  closeBackend :: store -> IO ()

  -- | clone a store handle for concurrent operations. the new handle must be
  -- safe to use concurrently with the original handle.
  --
  -- (e.g. open a second connection to the same postgresql database.)
  --
  -- this may be a no-op for backends which are already thread-safe (e.g.
  -- in-memory iorefs).
  cloneStore   :: store -> IO store

  -- Operations on interal keys

  -- | Allocate and return a new 'InternalKey'.
  createInternalKey :: store -> EntityName -> IO InternalKey

  -- | Find the 'InternalKey' associated with a 'ForeignKey'.
  lookupInternalKey :: store -> ForeignKey -> IO (Maybe InternalKey)

  -- | Delete an 'InternalKey' and any associated resources.
  deleteInternalKey :: store -> InternalKey -> IO Int


  -- Operations on foreign keys

  -- | Record a 'ForeignKey' and it's association with an 'InternalKey'.
  recordForeignKey :: store -> InternalKey -> ForeignKey -> IO ()

  -- | Find the 'ForeignKey' corresponding to an 'InternalKey'
  lookupForeignKey :: store -> SourceName -> InternalKey -> IO (Maybe ForeignKey)

  -- | Delete a 'ForeignKey'.
  deleteForeignKey :: store -> ForeignKey  -> IO Int

  -- | Delete all 'ForeignKey's associated with an 'InternalKey'.
  deleteForeignKeysWithInternal
    :: store -> InternalKey -> IO Int


  -- Operations on initial documents

  -- | Record the initial 'Document' associated with an 'InternalKey'.
  recordInitialDocument :: store -> InternalKey -> Document -> IO ()

  -- | Lookup the initial 'Document', if any, associated with an 'InternalKey'.
  lookupInitialDocument :: store -> InternalKey -> IO (Maybe Document)

  -- | Delete the initial 'Document', if any, associated with an 'InternalKey'.
  deleteInitialDocument :: store -> InternalKey -> IO Int


  -- Operations on patches

  -- | Record the success 'Diff' and a list of failed 'Diff's associated with a
  --   processed 'InternalKey'.
  recordDiffs         :: store -> InternalKey -> (Patch, [RejectedOp]) -> IO DiffID

  -- | Record that the conflicts in a 'Diff' are resolved.
  resolveDiffs        :: store -> DiffID -> IO ()

  -- | Lookup the list of 'Diff' IDs associated with an 'InternalKey'.
  lookupDiffIDs       :: store -> InternalKey -> IO [DiffID]

  -- | Lookup the list of conflicted 'Diff's with related information.
  lookupConflicts     :: store -> IO [ConflictResp]

  -- | Lookup the merged and conflicting 'Diff's with a given ID.
  lookupDiff          :: store -> DiffID -> IO (Maybe DiffResp)

  -- | Lookup the specified 'DiffOp's from the data store.
  lookupDiffConflicts :: store -> [OpID] -> IO [OpResp]

  -- | Delete the 'Diff', if any, with a given ID.
  deleteDiff          :: store -> DiffID -> IO Int

  -- | Delete the 'Diff's associated with an 'InternalKey'.
  deleteDiffsWithKey  :: store -> InternalKey -> IO Int


  -- Operation on store work queue

  -- | Add a work item to the work queue.
  addWork :: store -> WorkItem -> IO ()

  -- | Get a work item from the work queue.
  --   If the worker fails to complete the work, the desired behaviour
  --   is for the work item to be put back in the queue. This relies
  --   on the implementation.
  --
  getWork :: store -> IO (Maybe (WorkItemID, WorkItem))

  -- | Put a work item back in the work queue.
  ungetWork    :: store -> WorkItemID -> IO ()

  -- | Remove a completed work item from the queue.
  completeWork :: store -> WorkItemID -> IO ()


--------------------------------------------------------------------------------

-- | The identifier of a work item in the work queue.
type WorkItemID = Int

-- | An item of work to be stored in the work queue.
data WorkItem
    -- | A document was changed; process the update.
    = WorkNotify ForeignKey
    -- | A patch was submitted by a human; apply it.
    | WorkApplyPatch Int D.Patch
    deriving (Show, Eq)

instance ToJSON WorkItem where
    toJSON (WorkNotify fki) = object ["notify" .= fki]
    toJSON (WorkApplyPatch did new_diff) =
        object ["did" .= did, "diff" .= new_diff]

instance FromJSON WorkItem where
    parseJSON (Object v)
      =  (WorkNotify     <$> v .: "notify")
      <> (WorkApplyPatch <$> v .: "did" <*> v .: "diff")
    parseJSON _ = mzero
