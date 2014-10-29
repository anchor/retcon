--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

-- | Description: PostgreSQL storage for operational data.
--
-- Retcon maintains quite a lot of operational data. This implements the
-- operational data storage interface using a PostgreSQL database.
module Retcon.Store.PostgreSQL (PGStorage(..), prepareConfig, cleanupConfig) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Function
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.String
import Data.Text (Text)
import Database.PostgreSQL.Simple
import GHC.TypeLits
import System.Directory
import Text.Trifecta hiding (Success, token)
import qualified Text.Trifecta as P

import Retcon.Core
import Retcon.Diff
import Retcon.Options

import Utility.Configuration

-- | A persistent, PostgreSQL storage backend for Retcon.
data PGStorage = PGStore
    { unWrapConnection :: !Connection
    , unWrapConnString :: !ByteString
    }

joinSQL :: IsString x => [String] -> x
joinSQL = fromString . intercalate ";"

-- | Persistent PostgreSQL-backed data storage.
instance RetconStore PGStorage where

    storeInitialise opts = do
        let connstr = opts ^. optDB
        conn <- connectPostgreSQL connstr
        return $ PGStore conn connstr

    storeFinalise (PGStore conn _) =
        close conn

    storeClone (PGStore _ connstr) = do
        conn <- connectPostgreSQL connstr
        return $ PGStore conn connstr

    -- | Create a new 'InternalKey' by inserting a row in the database and
    -- using the allocated ID as the new key.
    storeCreateInternalKey :: forall entity. (RetconEntity entity)
                      => PGStorage
                      -> IO (InternalKey entity)
    storeCreateInternalKey (PGStore conn _) = do
        let entity = symbolVal (Proxy :: Proxy entity)
        res <- query conn "INSERT INTO retcon (entity) VALUES (?) RETURNING id" (Only entity)
        case res of
            [] -> error "Could not create new internal key"
            (Only key:_) -> return $ InternalKey key

    storeLookupInternalKey (PGStore conn _) fk = do
        results <- query conn "SELECT id FROM retcon_fk WHERE entity = ? AND source = ? AND fk = ? LIMIT 1" $ foreignKeyValue fk
        case results of
            Only key:_ -> return $ Just (InternalKey key)
            []         -> return Nothing

    storeDeleteInternalKey (PGStore conn _) ik = do
        let execute' sql = execute conn sql (internalKeyValue ik)
        d1 <- execute' "DELETE FROM retcon_initial WHERE entity = ? AND id = ?"
        d2 <- execute' "DELETE FROM retcon_diff WHERE entity = ? AND id = ?"
        d3 <- execute' "DELETE FROM retcon_fk WHERE entity = ? AND id = ?"
        d4 <- execute' "DELETE FROM retcon WHERE entity = ? AND id = ?"
        return . sum . map fromIntegral $ [d1, d2, d3, d4]

    storeRecordForeignKey (PGStore conn _) ik fk = do
        let (entity, source, fid) = foreignKeyValue fk
        let (_, iid) = internalKeyValue ik
        let values = (entity, iid, source, entity, iid, source, fid)
        -- TODO: This should be a real transaction or UPSERT or something.
        let sql = "DELETE FROM retcon_fk WHERE entity = ? AND id = ? AND source = ?; INSERT INTO retcon_fk (entity, id, source, fk) VALUES (?, ?, ?, ?)"
        void $ execute conn sql values

    storeDeleteForeignKey (PGStore conn _) fk = do
        let sql = "DELETE FROM retcon_fk WHERE entity = ? AND source = ? AND fk = ?"
        d <- execute conn sql $ foreignKeyValue fk
        return $ fromIntegral  d

    storeDeleteForeignKeys (PGStore conn _) ik = do
        let sql = "DELETE FROM retcon_fk WHERE entity = ? AND id = ?"
        d <- execute conn sql $ internalKeyValue ik
        return $ fromIntegral d

    storeLookupForeignKey :: forall entity source. (RetconDataSource entity source)
                     => PGStorage
                     -> InternalKey entity
                     -> IO (Maybe (ForeignKey entity source))
    storeLookupForeignKey (PGStore conn _) ik = do
        let source = symbolVal (Proxy :: Proxy source)
        let (entity, ik') = internalKeyValue ik
        let sql = "SELECT fk FROM retcon_fk WHERE entity = ? AND id = ? AND source = ?"
        res <- query conn sql (entity, ik', source)
        return $ case res of
            []         -> Nothing
            Only fk':_ -> Just $ ForeignKey fk'

    storeRecordInitialDocument (PGStore conn _) ik doc = do
        let (entity, ik') = internalKeyValue ik
        let sql = joinSQL [ "BEGIN"
                          , "DELETE FROM retcon_initial WHERE entity = ? AND id = ?"
                          , "INSERT INTO retcon_initial (id, entity, document) VALUES (?, ?, ?)"
                          , "COMMIT"
                          ]
        _ <- execute conn sql (entity, ik', ik', entity, toJSON doc)
        return ()

    storeLookupInitialDocument (PGStore conn _) ik = do
        let sql = "SELECT document FROM retcon_initial WHERE entity = ? AND id = ?"
        res <- query conn sql $ internalKeyValue ik
        case res of
            []       -> return Nothing
            Only v:_ -> case fromJSON v of
                Error _     -> return Nothing
                Success doc -> return . Just $ doc

    storeDeleteInitialDocument (PGStore conn _) ik = do
        let sql = "DELETE FROM retcon_initial WHERE entity = ? AND id = ?"
        d <- execute conn sql $ internalKeyValue ik
        return $ fromIntegral d

    storeRecordDiffs (PGStore conn _) ik (d, ds) = do
        let (entity, key) = internalKeyValue ik
        -- Use `void` to relabel the diffs and operations with () instead of
        -- the arbitrary, possibly unserialisable, labels.

        -- Extract the operations from the conflicting diffs.
        let ops = map (toJSON) . concatOf (traversed . to void . diffChanges) $ ds

        -- Record the merged diff in the database.
        [Only did] <- query conn diffQ (entity, key, not . null $ ops, encode . void $ d)

        -- Record conflicts in the database.
        void . executeMany conn opsQ . map (did,) $ ops
        return did
      where
        diffQ = "INSERT INTO retcon_diff (entity, id, is_conflict, content) VALUES (?, ?, ?, ?) RETURNING diff_id"
        opsQ = "INSERT INTO retcon_diff_conflicts (diff_id, content) VALUES (?, ?)"

    storeResolveDiff (PGStore conn _) did = do
        void $ execute conn sql (Only did)
      where
        sql = "UPDATE retcon_diff SET is_conflict = FALSE WHERE diff_id = ?"

    storeLookupDiff (PGStore conn _) diff_id = do
        let query' sql = query conn sql (Only diff_id)

        -- Load the merged diff.
        diff <- query' "SELECT entity, id, content FROM retcon_diff WHERE diff_id = ?"
        let diff' = map (\(entity, key, c) -> (entity,key,) <$> fromJSON c) diff

        -- Load the conflicting fragments.
        conflicts <- query' "SELECT content FROM retcon_diff_conflicts WHERE diff_id = ?"
        let conflicts' = map fromSuccess . filter isSuccess . map (fromJSON . fromOnly) $ conflicts

        return $ case diff' of
            Success (entity,key,d):_ -> Just ((entity,key), d, conflicts')
            _           -> Nothing
      where
        fromSuccess (Success a) = a
        fromSuccess _ = error "fromSuccess: Cannot unwrap not-a-success."
        isSuccess (Success _) = True
        isSuccess _ = False

    -- | Lookup the list of conflicted 'Diff's with related information.
    storeLookupConflicts (PGStore conn _) = do
        diffs <- query_ conn diffS
        ops <- query_ conn opsS
        return $ map (match ops) diffs
      where
        -- Filter the operations which correspond to a diff and add them to the
        -- tuple.
        --
        -- TODO This is O(mn). I am embarrassing.
        match all_ops (doc, diff, diff_id) =
            let ops = map (\(_, op_id, op) -> (op_id, op))
                    . filter (\(op_diff_id,_,_) -> diff_id == op_diff_id)
                    $ all_ops
            in (doc, diff, diff_id, ops)
        diffS = "SELECT CAST(doc.document AS TEXT), CAST(diff.content AS TEXT), diff.id "
            <> "FROM retcon_diff AS diff "
            <> "JOIN retcon_initial AS doc "
            <> "ON (diff.entity = doc.entity AND diff.id = doc.id) "
            <> "WHERE diff.is_conflict ORDER BY diff.id ASC"
        opsS = "SELECT op.diff_id, op.operation_id, CAST(op.content AS TEXT) "
            <> "FROM retcon_diff_conflicts AS op "
            <> "LEFT JOIN retcon_diff AS diff ON (op.diff_id = diff.diff_id) "
            <> "WHERE diff.is_conflict ORDER BY op.diff_id ASC"

    storeLookupDiffConflicts (PGStore conn _) ids = do
        (map parse) <$> (query conn sql . Only . In $ ids)
      where
        sql = "SELECT diff_id, operation_id, content FROM retcon_diff_conflicts WHERE operation_id IN ?"
        parse :: (Int, Int, Value) -> (Int, Int, DiffOp ())
        parse (did, opid, op_json) = do
            case fromJSON op_json of
                Success dop -> (did, opid, dop)
                Error e     -> error $
                   "Could not load diff operation: " <> show (did,opid) <> " " <>
                   e

    storeLookupDiffIds (PGStore conn _) ik = do
        r <- query conn "SELECT diff_id FROM retcon_diff WHERE entity = ? AND id = ?" $ internalKeyValue ik
        return . map fromOnly $ r

    storeDeleteDiff (PGStore conn _) diff_id = do
        let execute' sql = execute conn sql (Only diff_id)
        ops <- execute' "DELETE FROM retcon_diff_conflicts WHERE diff_id = ?"
        d <- execute' "DELETE FROM retcon_diff WHERE diff_id = ?"
        return . sum . map fromIntegral $ [ops, d]

    storeDeleteDiffs (PGStore conn _) ik = do
        let execute' sql = execute conn sql (internalKeyValue ik)
        d1 <- execute' "DELETE FROM retcon_diff_conflicts WHERE diff_id IN (SELECT diff_id FROM retcon_diff WHERE entity = ? AND id = ?)"
        d2 <- execute' "DELETE FROM retcon_notifications WHERE entity = ? AND id = ?"
        d3 <- execute' "DELETE FROM retcon_diff WHERE entity = ? AND id = ?"
        return . sum . map fromIntegral $ [d1, d2, d3]

    storeRecordNotification (PGStore conn _) ik did = do
        let (entity, eid) = internalKeyValue ik
        void $ execute conn sql (entity, eid, did)
      where
        sql = "INSERT INTO retcon_notifications (entity, id, diff_id) VALUES (?, ?, ?)"

    storeFetchNotifications (PGStore conn _) limit = do
        (ids :: [Only Int]) <- query conn sqlI (Only limit)
        let ids' = map fromOnly ids
        let least = minimum ids'
        let greatest = maximum ids'
        notifications <- query conn sqlN (least, greatest)
        [Only remaining] <- query conn sqlD (least, greatest)
        return (remaining, notifications)
      where
        sqlI = "SELECT id FROM retcon_notifications ORDER BY created ASC LIMIT ?"
        sqlN = "SELECT created, entity, key, diff_id FROM retcon_notifications WHERE id >= ? AND id <= ?"
        sqlD = "DELETE FROM retcon_notifications WHERE id >= ? AND id <= ?; SELECT count(*) FROM retcon_notifications"

    storeAddWork (PGStore conn _) work = do
        let content = toJSON work
        void $ execute conn sql (Only content)
      where
        sql = "INSERT INTO retcon_workitems (content) VALUES (?)"

    storeGetWork (PGStore conn _) = do
        res <- listToMaybe <$> query_ conn sql
        case res of
           Just (work_id, work) -> case fromJSON work of
               Success x -> return (Just (work_id, x))
               _         -> return Nothing
           _ -> return Nothing
      where
        sql = "UPDATE retcon_workitems SET state = 'processing' " <>
              "WHERE " <>
                  "id = (SELECT id FROM retcon_workitems " <>
                        "WHERE state = 'new' ORDER BY id ASC LIMIT 1) " <>
                  "AND state = 'new' " <>
              "RETURNING id, content;"

    storeCompleteWork (PGStore conn _) work_id =
        void $ execute conn sql (Only work_id)
      where
        sql = "DELETE FROM retcon_workitems WHERE id = ?"

-- | Load the parameters from the path specified in the options.
prepareConfig
    :: (RetconOptions, [Text])
    -> [SomeEntity]
    -> IO (RetconConfig SomeEntity RWToken)
prepareConfig (opt, event) entities = do
    params <- maybe (return mempty) readParams $ opt ^. optParams
    store :: PGStorage <- storeInitialise opt
    -- entities' <- initialiseEntities params entities
    return $ RetconConfig
        (opt ^. optVerbose)
        (opt ^. optLogging)
        (token store)
        params
        event
        entities
  where
    readParams :: FilePath -> IO (Map (Text, Text) (Map Text Text))
    readParams path = do
        exists <- doesFileExist path
        results <- if exists
            then parseFromFileEx configParser path
            else error $ "specified config file doesn not exist: \"" ++ path ++ "\""
        case results of
            P.Success results' -> return $ convertConfig results'
            P.Failure failure -> error $ show failure

cleanupConfig
    :: RetconConfig SomeEntity RWToken
    -> IO ()
cleanupConfig cfg = do
    -- void $ finaliseEntities (cfg ^. cfgParams) $ cfg ^. cfgEntities
    void $ closeToken (cfg ^. cfgDB)
