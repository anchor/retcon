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

-- | Description: Unique identifiers for internal an external use.
module Retcon.Identifier (
    -- * Configuration names
    EntityName(..),
    SourceName(..),
    -- * Unique identifiers
    InternalID, ForeignID,
    ForeignKey(..),
    InternalKey(..),
    -- * Checking compatibility
    Synchronisable(..),
    compatibleEntity,
    compatibleSource,
) where

import           Data.Aeson.TH
import           Data.String
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow


-- | Unique name for an entity.
newtype EntityName = EntityName { ename :: Text }
  deriving (Eq, Ord)

instance IsString EntityName where
    fromString = EntityName . T.pack

instance Show EntityName where
    show = T.unpack . ename

instance Read EntityName where
    readsPrec _ s = [(EntityName (T.pack s), "")]

-- | Unique name for a data source.
newtype SourceName = SourceName { sname :: Text }
  deriving (Eq, Ord)

instance Show SourceName where
    show = T.unpack . sname

instance IsString SourceName where
    fromString = SourceName . T.pack

instance Read SourceName where
    readsPrec _ s = [(SourceName (T.pack s), "")]

-- | Types which participate, in one way or another, in the synchronisation
-- process.
class Synchronisable a where
    -- | Get the 'EntityName' for which the value is valid.
    getEntityName :: a -> EntityName

    -- | Get the 'SourceName' for which the value is valid.
    getSourceName :: a -> SourceName

--------------------------------------------------------------------------------

type InternalID = Int
type ForeignID  = Text

-- | Uniquely identify a 'Document' shared across one or more 'DataSource's.
--
-- Each 'InternalKey' value can be mapped to the 'ForeignKey's for the
-- 'DataSource's which store copies of the associated 'Document'.
data InternalKey = InternalKey
    { ikEntity :: EntityName
    , ikID     :: InternalID
    } deriving (Eq, Ord, Show)

instance Synchronisable InternalKey where
    getEntityName   = ikEntity
    getSourceName _ = SourceName ""

-- | Uniquely identify a 'Document' stored in 'DataSource'.
data ForeignKey = ForeignKey
    { fkEntity :: EntityName
    , fkSource :: SourceName
    , fkID     :: ForeignID
    } deriving (Eq, Ord, Show)

instance Synchronisable ForeignKey where
    getEntityName = fkEntity
    getSourceName = fkSource

-- json

$(deriveJSON defaultOptions ''EntityName)
$(deriveJSON defaultOptions ''SourceName)
$(deriveJSON defaultOptions ''ForeignKey)

-- postgres

instance ToField EntityName where
  toField (EntityName n) = toField n

instance ToField SourceName where
  toField (SourceName n) = toField n

instance ToRow ForeignKey where
  toRow (ForeignKey a b c) = toRow (a,b,c)

instance ToRow InternalKey where
  toRow (InternalKey a b) = toRow (a,b)

--------------------------------------------------------------------------------

-- | Check that two synchronisable values have the same entity.
compatibleEntity
    :: (Synchronisable a, Synchronisable b)
    => a
    -> b
    -> Bool
compatibleEntity a b = getEntityName a == getEntityName b

-- | Check that two synchronisable values have the same data source.
compatibleSource
    :: (Synchronisable a, Synchronisable b)
    => a
    -> b
    -> Bool
compatibleSource a b = compatibleEntity a b && getSourceName a == getSourceName b

