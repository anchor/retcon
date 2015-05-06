--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Description: Represent and operate on retcond documents.
--
-- A 'Document' is, essentially, a JSON 'Value' together with some metadata
-- describing its type, system of origin, etc.
module Retcon.Document (
    Document(..),
    documentEntity,
    documentSource,
    documentContent,

    emptyDocument,
    calculateInitialDocument,
) where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.List
import           Data.Monoid

import           Retcon.Identifier

-- | A JSON 'Value' from a particular 'Entity'.
data Document = Document
    { _documentEntity  :: EntityName -- ^ Type of data.
    , _documentSource  :: SourceName -- ^ System of origin.
    , _documentContent :: Value      -- ^ Document content.
    }
  deriving (Eq, Show)
makeLenses ''Document

instance ToJSON Document where
  toJSON = _documentContent

instance FromJSON Document where
  parseJSON x = return $ Document "" "" x

instance Synchronisable Document where
    getEntityName = _documentEntity
    getSourceName = _documentSource

--------------------------------------------------------------------------------

-- | Construct an empty 'Document'.
emptyDocument
    :: EntityName
    -> SourceName
    -> Document
emptyDocument e s = Document e s Null

-- | Construct an initial 'Document' for use in identifying and processing
-- changes.
--
-- Reports an error when some or all documents have conflicting entities or
-- sources.
calculateInitialDocument
    :: [Document]
    -> Either String Document
calculateInitialDocument docs = do
    entity <- determineEntity docs
    checkSources docs
    case docs of
        []  -> bail "No documents provided."
        [d] -> Right $ d & documentEntity .~ entity
                         & documentSource .~ "<initial>"
        _   -> bail "Too many documents provided."
  where
    bail m = Left $ "Cannot calculate initial document: " <> m
    determineEntity ds = case nub . fmap (view documentEntity) $ ds of
        [] -> bail "No documents"
        [e] -> Right e
        l -> bail $ "Types do not match (" <> show l <> ")"
    -- Check that the documents all come from different sources
    checkSources ds =
        let all_sources = fmap (view documentSource) ds
            uniq_sources = group . sort $ all_sources
        in when (any (\x -> length x > 1) uniq_sources) $
            bail "Multiple documents from same data source."
