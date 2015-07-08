{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Description: Types defining the protocol for client/server interaction
-- with retcond.
module Retcon.Network.Protocol where

import           Control.Applicative
import           Control.Exception
import           Control.Lens.TH
import qualified Data.Aeson          as Aeson
import qualified Data.Aeson.Diff     as Diff
import           Data.Binary
import qualified Data.ByteString     as BS
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Data.Typeable

import           Retcon.Document
import           Retcon.Identifier
import           Retcon.Store

-------------------------------------------------------------------------------
-- * Errors

-- | Values describing error states of the retcon API.
data APIError
    = UnknownServerError
    | TimeoutError
    | DecodeError
    | InvalidNumberOfMessageParts
    | UnknownKeyError -- ^ Notification contained an unknown key.
  deriving (Show, Eq, Typeable)

instance Exception APIError

instance Enum APIError where
    fromEnum TimeoutError = 0
    fromEnum InvalidNumberOfMessageParts = 1
    fromEnum DecodeError = 2
    fromEnum UnknownKeyError = 3
    fromEnum UnknownServerError = maxBound

    toEnum 0 = TimeoutError
    toEnum 1 = InvalidNumberOfMessageParts
    toEnum 2 = DecodeError
    toEnum 3 = UnknownKeyError
    toEnum _ = UnknownServerError

-------------------------------------------------------------------------------
-- * Network messages

-- | Message headers for network protocol.
data Header request response where
    -- Get a list of unresolved conflicts.
    HeaderConflicted :: Header RequestConflicted ResponseConflicted
    -- Process a change
    HeaderChange :: Header RequestChange ResponseChange
    -- Resolve an unresolved conflict.
    HeaderResolve :: Header RequestResolve ResponseResolve
    -- Invalid request/response.
    InvalidHeader :: Header InvalidRequest InvalidResponse

instance Enum SomeHeader where
    fromEnum (SomeHeader HeaderConflicted) = 0
    fromEnum (SomeHeader HeaderChange) = 1
    fromEnum (SomeHeader HeaderResolve) = 2
    fromEnum (SomeHeader InvalidHeader) = maxBound

    toEnum 0 = SomeHeader HeaderConflicted
    toEnum 1 = SomeHeader HeaderChange
    toEnum 2 = SomeHeader HeaderResolve
    toEnum _ = SomeHeader InvalidHeader

data SomeHeader where
    SomeHeader
        :: Header request response
        -> SomeHeader

-- ** List unresolved conflicts

-- | Request a list of conflicts.
data RequestConflicted = RequestConflicted
  deriving (Eq, Show)

data ResponseConflictedItem = ResponseConflictedItem
    { _conflictDocument :: Document
    , _conflictPatch    :: Diff.Patch
    , _conflictDiffID   :: DiffID
    , _conflictOps      :: [(OpID, Diff.Operation)]
    } deriving (Eq, Show)

instance Binary ResponseConflictedItem where
  put (ResponseConflictedItem d p i o) = put d >> put p >> put i >> put o
  get = ResponseConflictedItem <$> get <*> get <*> get <*> get

data ResponseConflictedSerialisedItem = ResponseConflictedSerialisedItem
    { _conflictDocument' :: BS.ByteString
    , _conflictPatch'    :: BS.ByteString
    , _conflictDiffID'   :: Int
    , _conflictOps'      :: [(Int, BS.ByteString)]
    } deriving (Eq, Show)

instance Binary ResponseConflictedSerialisedItem where
  put (ResponseConflictedSerialisedItem d p i o) = put d >> put p >> put i >> put o
  get = ResponseConflictedSerialisedItem <$> get <*> get <*> get <*> get

-- | Response containing a list of unresolved conflicts.
data ResponseConflicted
    = ResponseConflicted [ResponseConflictedItem]
    -- | Pre-serialised version of the same data. This is generated on the server
    -- to avoid the overhead of de-serialising from the database only to serialise
    -- immediately. Woo.
    | ResponseConflictedSerialised [ResponseConflictedSerialisedItem]
  deriving (Eq, Show)

instance Binary RequestConflicted where
    put _ = return ()
    get = return RequestConflicted

instance Binary ResponseConflicted where
    put (ResponseConflicted ds) = put ds
    put (ResponseConflictedSerialised ds) = put ds
    get = ResponseConflicted <$> get

-- ** Resolve outstanding conflict

data RequestResolve = RequestResolve DiffID [OpID]
  deriving (Eq, Show)

data ResponseResolve = ResponseResolve
  deriving (Eq, Show)

instance Binary RequestResolve where
    put (RequestResolve did conflicts) = put (did, conflicts)
    get = do
        (did, conflicts) <- get
        return $ RequestResolve did conflicts

instance Binary ResponseResolve where
    put _ = return ()
    get = return ResponseResolve

-- ** Processing a change

data RequestChange = RequestChange ChangeNotification
  deriving (Eq, Show)

data ResponseChange = ResponseChange
  deriving (Eq, Show)

instance Binary RequestChange where
    put (RequestChange (ChangeNotification entity source fk)) =
        put (entity, source, fk)
    get = do
        (entity, source, fk) <- get
        return . RequestChange $ ChangeNotification entity source fk

instance Binary ResponseChange where
    put _ = return ()
    get = return ResponseChange

-- ** Invalid request

data InvalidRequest = InvalidRequest
  deriving (Eq, Show)

data InvalidResponse = InvalidResponse
  deriving (Eq, Show)

instance Binary InvalidRequest where
    put _ = return ()
    get = return InvalidRequest

instance Binary InvalidResponse where
    put _ = return ()
    get = return InvalidResponse

-------------------------------------------------------------------------------
-- * Types

-- | A notification for Retcon that the document with 'ForeignID' which is an
-- 'EntityName' at the data source 'SourceName' has changed in some way.
data ChangeNotification = ChangeNotification
    { _notificationEntity    :: EntityName
    , _notificationSource    :: SourceName
    , _notificationForeignID :: ForeignID
    }
  deriving (Eq, Show)

makeLenses ''ChangeNotification

instance Binary T.Text where
    put = put . T.encodeUtf8
    get = T.decodeUtf8 <$> get

instance Binary EntityName where
    put (EntityName n) = put n
    get = EntityName <$> get

instance Binary SourceName where
    put (SourceName n) = put n
    get = SourceName <$> get

instance Binary Diff.Patch where
    put = put . Aeson.encode
    get = getJSON

instance Binary Diff.Operation where
    put = put . Aeson.encode
    get = getJSON

instance Binary Document where
    put = put . Aeson.encode
    get = getJSON

-- | Get and decode a value crammed into Binary as a JSON 'Value'.
getJSON
    :: Aeson.FromJSON a
    => Get a
getJSON = do
    json <- Aeson.eitherDecode <$> get
    case json of
        Right x -> return x
        Left msg -> fail msg
