--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Description: Diff and patch /Retcon/ documents.
module Retcon.Diff
     ( -- * Operations
       diff
     , patch
     , merge
     , extractLabel

       -- * Merge policies
     , MergePolicy
     , doNothing
     , acceptAll
     , rejectAll
     , ignoreConflicts
     , trustOnlySource

       -- * Representing patches
     , PatchLabel(..)
     , Patch(..)
     , patchLabel
     , patchDiff
     , emptyPatch
     , RejectedOp(..)
     , rejectedLabel
     , rejectedOperation
     ) where

import           Control.Applicative
import           Control.Lens        hiding ((.=))
import qualified Data.Aeson.Diff     as D
import qualified Data.Map            as M
import           Data.Monoid

import           Retcon.Document
import           Retcon.Identifier


data PatchLabel
  = Unamed           -- ^ ()
  | Name SourceName  -- ^ Named after its origin.
  deriving (Eq, Show)

data Patch = Patch
  { _patchLabel :: PatchLabel
  , _patchDiff  :: D.Patch
  } deriving (Eq, Show)
makeLenses ''Patch

data RejectedOp = RejectedOp
  { _rejectedLabel     :: PatchLabel
  , _rejectedOperation :: D.Operation
  } deriving (Eq, Show)
makeLenses ''RejectedOp

-- | Describes the way we merge diffs.
--
data MergePolicy = MergePolicy
  { extractLabel :: Document -> PatchLabel
  , merge        :: Patch    -> Patch -> MergeResult }

type MergeResult = (Patch, [RejectedOp])

emptyPatch :: Patch
emptyPatch = Patch Unamed mempty

--------------------------------------------------------------------------------

-- | Policy: do nothing.
--
--   Be a lazy bum.
doNothing :: MergePolicy
doNothing = MergePolicy {..}
  where
    extractLabel   = const Unamed
    merge      _ _ = (emptyPatch, mempty)

-- | Policy: accept all changes.
--
--   All changes will be applied in whatever arbitrary order they are encountered.
acceptAll :: MergePolicy
acceptAll = MergePolicy {..}
  where
    extractLabel = const Unamed
    merge p1 p2  = (Patch Unamed $ (p1 ^. patchDiff) <> (p2 ^. patchDiff), mempty)

-- | Policy: reject all changes.
--
--   All input are rejected and the merged diff is empty.
rejectAll :: MergePolicy
rejectAll = MergePolicy {..}
  where
    extractLabel = const Unamed
    merge p1 p2  = (Patch Unamed mempty, reject p1 <> reject p2)

-- | Trust only patches from a particular 'DataSource', discarding all other
-- changes.
trustOnlySource :: SourceName -> MergePolicy
trustOnlySource (Name -> name) = MergePolicy {..}
  where
    extractLabel d = Name $ d ^. documentSource
    merge p1 p2
      | name == (p2 ^. patchLabel) = (p2, reject p1)
      | name == (p1 ^. patchLabel) = (p1, reject p2)
      | otherwise = ( Patch name mempty
                    , reject p1 <> reject p2 )

-- | Policy: reject all conflicting changes.
--
--   Changes will be applied iff they are the sole change affecting that key; all
--   other changes will be rejected.
ignoreConflicts :: MergePolicy
ignoreConflicts = MergePolicy {..}
  where
    extractLabel = const Unamed
    merge p1 p2
      = let m1 = toMap p1
            m2 = toMap p2
            allOps    = M.unionWith (++) m1 m2
            conflicts = M.intersectionWith (++) m1 m2
            accepts   = M.difference allOps conflicts
        in  (fromMap accepts, justOps conflicts)

    justOps = map (RejectedOp Unamed)       . concat . M.elems
    fromMap = foldr addOperation emptyPatch . concat . M.elems

    -- Groups changes in a patch by the change path.
    toMap   = foldr count M.empty . D.patchOperations . _patchDiff
    count o = M.insertWith (++) (D.changePath o) [o]

--------------------------------------------------------------------------------

-- | Use a 'MergePolicy' to compare two versions of a document and extract a
-- 'Patch' describing changes.
diff
    :: MergePolicy
    -> Document
    -> Document
    -> Patch
diff MergePolicy{..} d1 d2 =
    let j1 = d1 ^. documentContent
        j2 = d2 ^. documentContent
        l = extractLabel d2
        d = D.diff j1 j2
    in Patch l d

-- | Use a 'MergePolicy' to apply a 'Patch' to a 'Document'.
patch
    :: MergePolicy
    -> Patch
    -> Document
    -> Document
patch MergePolicy{} p d = d & documentContent %~ D.patch (p ^. patchDiff)

--------------------------------------------------------------------------------

-- * Utility

-- | Convert all the 'D.Operation's in a 'D.Patch' into 'RejectedOp's.
reject :: Patch -> [RejectedOp]
reject p = fmap (RejectedOp (p ^. patchLabel)) . D.patchOperations $ p ^. patchDiff

addOperation :: D.Operation -> Patch -> Patch
addOperation x p = p & patchDiff . patchOperations <>~ [x]

patchOperations :: Lens' D.Patch [D.Operation]
patchOperations f (D.Patch os) = D.Patch <$> f os
