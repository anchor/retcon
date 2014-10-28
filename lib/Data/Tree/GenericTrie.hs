--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- |
-- Description: A trie with arbitrary key values.
--
-- This module defines a generic trie-like data structure where keys are
-- sequences of any type in 'Ord'.

{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.Tree.GenericTrie where

import Control.Applicative
import Control.Lens
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import GHC.Exts (IsList (..))
import Prelude hiding (concatMap, foldl, foldr, lookup)

-- | A tree similar to 'Data.Tree' except it uses a 'Map' of children
-- instead a list.
data Tree key value
    = Node { _nodeValue    :: Maybe value
           , _nodeChildren :: Map key (Tree key value)
           }
  deriving (Eq, Read, Show, Functor, Traversable, Foldable)
makeLenses ''Tree

instance Monoid (Tree a b) where
    mempty = Node Nothing M.empty
    mappend = error "Monoid(Tree a b): union not implemented"

instance FunctorWithIndex [i] (Tree i)
instance FoldableWithIndex [i] (Tree i)
instance TraversableWithIndex [i] (Tree i) where
    itraverse f = go []
      where
        -- go :: [i] -> Tree i a -> f (Tree i a)
        go idx (Node Nothing child_map) =
            Node Nothing <$> itraverse (appendIndex idx) child_map

        go idx (Node (Just v) child_map) =
            Node <$> fmap Just (f idx v)
                 <*> itraverse (appendIndex idx) child_map

        appendIndex idx i = go (idx ++ [i])

-- Plated traverses a Node's immediate children.
instance Plated (Tree k v) where
    plate f (Node v childs) = Node v <$> traverse f childs


instance (Ord k) => IsList (Tree k v) where
  type Item (Tree k v) = ([k],v)
  fromList [] = mempty
  fromList kvs = foldl worker mempty kvs
    where
        worker (Node _ kids) ([]  ,v) = Node (Just v) kids
        worker (Node l kids) (k:ks,v) = Node l $ M.alter (update (ks,v)) k kids
        update vs       Nothing = update vs $ Just mempty
        update ([],v)   (Just (Node _ ch)) = Just $ Node (Just v) ch
        update (k:ks,v) (Just (Node l ch)) =
            let emptyCase = update (ks,v) $ Just mempty
                fullCase n = update (ks,v) $ Just n
                ch' = M.alter (maybe emptyCase fullCase) k ch
            in Just $ Node l ch'

  toList t = t ^@.. itraversed

-- Ixed instance
type instance Index (Tree key value) = [key]

type instance IxValue (Tree k a) = a
instance Ord key => Ixed (Tree key value) where
  ix k f m = case lookup k m of
       Just v  -> f v <&> \v' -> insert k v' m
       Nothing -> pure m

emptyNode :: Tree k v -> Bool
emptyNode (Node l kids) = isNothing l && M.null kids

-- | Filter a Tree by a predicate.
filterTree :: (Tree k v -> Bool) -> Tree k v -> Tree k v
filterTree p = rewrite f
  where
    f t@(Node x childs) =
        if M.null . M.filter emptyNode $ childs
            then if p t then Nothing else Just (Node Nothing childs)
            else Just (Node x (M.filter (not . emptyNode) childs))

-- | Prune a Tree, removing empty nodes.
pruneTree :: Tree k v -> Tree k v
pruneTree = pruneTree1 . (nodeChildren %~ M.filter (not . emptyNode . pruneTree))

-- | Prune a Tree, removing empty nodes.
pruneTree1 :: Tree k v -> Tree k v
pruneTree1 = nodeChildren %~ M.filter (not . emptyNode)

-- | Follow a path and return the value, if any, at the end.
lookup
    :: Ord k
    => [k]
    -> Tree k v
    -> Maybe v
lookup []     (Node v _kids) = v
lookup (n:ns) (Node _ kids ) = M.lookup n kids >>= lookup ns

insert
    :: Ord k
    => [k]
    -> v
    -> Tree k v
    -> Tree k v
insert [] v (Node _ kids) =
    Node (Just v) kids
insert (n:ns) v node =
    node & nodeChildren . at n . anon mempty emptyNode %~ insert ns v


delete
    :: Ord k
    => [k]
    -> Tree k v
    -> Tree k v
delete [] = pruneTree1 . (nodeValue .~ Nothing)
delete (k:ks) =
     pruneTree1 . (nodeChildren . at k . traversed %~ delete ks)


instance Ord k => At (Tree k a) where
    at k f m = f mv <&> \r -> case r of
        Nothing -> maybe m (const (delete k m)) mv
        Just v' -> insert k v' m
      where mv = lookup k m
