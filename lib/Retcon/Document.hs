--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Description : Represent documents which will be processed by retcon.
--
-- This module implements the 'Document' data type which the retcon
-- system manipulates. Documents are, essentially, nested key/value
-- maps.

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Retcon.Document (
    Document (..),
    DocumentKey,
    DocumentValue,
    FromJSON,
    ToJSON,
    mkNode,
    calculateInitialDocument
) where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Lens.TH
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Lazy as H hiding (lookup)
import Data.HashMap.Strict (lookup)
import Data.List hiding (lookup)
import qualified Data.Map as M
import Data.Monoid
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Tree.GenericTrie (Tree (..))
import GHC.Exts (IsList (..))
import Prelude hiding (lookup)

type DocumentKey =  Text
type DocumentValue = Text

-- | A retcon 'Document' is an edge-labelled tree with 'Text' labels on
-- both nodes and edges.
newtype Document
    = Document { unDocument :: Tree DocumentKey DocumentValue }
  deriving (Monoid, Eq, Show)
makeWrapped ''Document


mkNode
    :: Applicative f
    => Maybe DocumentValue
    -> f Document
mkNode x = pure . Document $ Node x mempty

-- | Translates any kind of JSON object to a Document.
instance FromJSON Document where
    parseJSON (String str) = mkNode $ Just str
    parseJSON (Number num) = mkNode . Just . T.pack $ show num
    parseJSON (Bool True)  = mkNode $ Just "TRUE"
    parseJSON (Bool False) = mkNode $ Just "FALSE"
    parseJSON (Null)       = mkNode Nothing
    parseJSON (Array _)    = mzero -- TODO Maybe convert into a map?
    parseJSON (Object v)   = docFromJsonObject (Object v)

-- | Translates a top-level document to JSON.
instance ToJSON Document where
    toJSON doc = docToJSON doc 0

-- | Translate a JSON Object to a Document.
docFromJsonObject
    :: Value
    -> Parser Document
docFromJsonObject (Object v) =
    case identKey "_topLevelValue" of
        Nothing  -> standardObj v
        Just tlv ->
            case identKey "_contents" of
                Nothing          -> standardObj v
                Just (Object v') -> specialObj tlv v'
  where
    identKey s = lookup s v
    standardObj v = Document . Node Nothing . M.fromList . H.toList <$>
        traverse (\x -> unDocument <$> parseJSON x) v
    specialObj Null v = Document . Node Nothing . M.fromList . H.toList <$>
        traverse (\x -> unDocument <$> parseJSON x) v
    specialObj (String tlv) v = Document . Node (Just tlv) . M.fromList . H.toList <$>
        traverse (\x -> unDocument <$> parseJSON x) v

-- | Translate a document to JSON
-- Allowing different behaviours at top level than subsequent levels.
-- Top level documents contain its value in a _topLevelValue element,
-- and any children in a _contents list.
docToJSON
    :: Document
    -> Int
    -> Value
docToJSON (Document (Node nv childs)) 0 = object [
    "_topLevelValue" .= tlVal,
    "_contents"      .= contents ]
  where
    tlVal = case nv of
        Nothing -> Null
        Just v' -> String v'
    contents = object $ map (\(k,v) -> k .= docToJSON (Document v) 1) $ M.toAscList childs
docToJSON (Document (Node Nothing childs)) l = object $ map (\(k,v) -> k .= d' v) $ M.toAscList childs
  where
    d' v = docToJSON (Document v) (l + 1)
docToJSON (Document (Node (Just val) childs)) l
    | M.null childs = toJSON val
    | otherwise     = object $ map (\(k,v) -> k .= d' v) $ M.toAscList childs
  where
    d' v = docToJSON (Document v) (l + 1)

-- | Calulate an "initial" document from a collection of input documents.
--
-- Currently takes the keys and values upon which all input documents agree.
calculateInitialDocument
    :: [Document]
    -> Document
calculateInitialDocument docs =
    let count = length docs
        pairs = sort . concatMap (toList . unDocument) $ docs
        common = map head . filter (\l -> length l == count) . group $ pairs
    in Document $ fromList common

