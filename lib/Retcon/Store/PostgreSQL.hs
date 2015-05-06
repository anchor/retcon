{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Retcon.Store.PostgreSQL
     ( PGStore(..)
     , StoreOpts(..)
     ) where

import           Control.Applicative
import           Control.Lens               hiding (op)
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Diff            ()
import           Data.ByteString            (ByteString)
import qualified Data.List                  as L
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Database.PostgreSQL.Simple

import           Retcon.Diff                hiding (diff)
import           Retcon.Identifier
import           Retcon.Store.Base          hiding (ops)

data PGStore = PGStore
    { pgconn    :: !Connection
    , pgconnstr :: !ByteString
    }

sqlConcat :: IsString x => [String] -> x
sqlConcat = fromString . L.intercalate ";"

-- | Persistent PostgreSQL-backed data storage.
instance Store PGStore where
  newtype StoreOpts PGStore = PGOpts { connstr :: ByteString }

  initBackend (connstr -> str) = do
      conn <- connectPostgreSQL str
      return $ PGStore conn str

  closeBackend = close . pgconn

  cloneStore (PGStore _ str) = do
      conn <- connectPostgreSQL str
      return $ PGStore conn str

  -- | Create a new 'InternalKey' by inserting a row in the database and
  -- using the allocated ID as the new key.
  createInternalKey (PGStore conn _) entity = do
      res <- query conn "INSERT INTO retcon (entity) VALUES (?) RETURNING id" (Only entity)
      case res of
          [] -> error "Could not create new internal key"
          (Only key:_) -> return $ InternalKey entity key

  lookupInternalKey (PGStore conn _) fk = do
      results <- query conn "SELECT id FROM retcon_fk WHERE entity = ? AND source = ? AND fk = ? LIMIT 1" fk
      case results of
          Only key:_ -> return $ Just (InternalKey (fkEntity fk) key)
          []         -> return Nothing

  deleteInternalKey (PGStore conn _) ik = do
      let execute' sql = execute conn sql ik
      d1 <- execute' "DELETE FROM retcon_initial WHERE entity = ? AND id = ?"
      d2 <- execute' "DELETE FROM retcon_diff WHERE entity = ? AND id = ?"
      d3 <- execute' "DELETE FROM retcon_fk WHERE entity = ? AND id = ?"
      d4 <- execute' "DELETE FROM retcon WHERE entity = ? AND id = ?"
      return . sum . fmap fromIntegral $ [d1, d2, d3, d4]

  recordForeignKey (PGStore conn _) ik fk = do
      let entity = fkEntity fk
          source = fkSource fk
          fid    = fkID     fk
          iid    = ikID     ik
          values = (entity, iid, source, entity, iid, source, fid)

      -- TODO: This should be a real transaction or UPSERT or something.
      let sql = "DELETE FROM retcon_fk WHERE entity = ? AND id = ? AND source = ?; INSERT INTO retcon_fk (entity, id, source, fk) VALUES (?, ?, ?, ?)"
      void $ execute conn sql values

  deleteForeignKey (PGStore conn _) fk = do
      let sql = "DELETE FROM retcon_fk WHERE entity = ? AND source = ? AND fk = ?"
      d <- execute conn sql fk
      return $ fromIntegral  d

  deleteForeignKeysWithInternal (PGStore conn _) ik = do
      let sql = "DELETE FROM retcon_fk WHERE entity = ? AND id = ?"
      d <- execute conn sql ik
      return $ fromIntegral d

  lookupForeignKey (PGStore conn _) source ik = do
      let entity = ikEntity ik
          ik'    = ikID     ik
          sql    = "SELECT fk FROM retcon_fk WHERE entity = ? AND id = ? AND source = ?"
      res <- query conn sql (entity, ik', source)
      return $ case res of
          []         -> Nothing
          Only fk':_ -> Just $ ForeignKey entity source fk'

  recordInitialDocument (PGStore conn _) ik doc = do
      let entity = ikEntity ik
          ik'    = ikID     ik
          sql    = sqlConcat
                 [ "BEGIN"
                 , "DELETE FROM retcon_initial WHERE entity = ? AND id = ?"
                 , "INSERT INTO retcon_initial (id, entity, document) VALUES (?, ?, ?)"
                 , "COMMIT"
                 ]
      _ <- execute conn sql (entity, ik', ik', entity, toJSON doc)
      return ()

  lookupInitialDocument (PGStore conn _) ik = do
      let sql = "SELECT document FROM retcon_initial WHERE entity = ? AND id = ?"
      res <- query conn sql ik
      case res of
          []       -> return Nothing
          Only v:_ -> case fromJSON v of
              Error _     -> return Nothing
              Success doc -> return . Just $ doc

  deleteInitialDocument (PGStore conn _) ik = do
      let sql = "DELETE FROM retcon_initial WHERE entity = ? AND id = ?"
      d <- execute conn sql ik
      return $ fromIntegral d

  recordDiffs (PGStore conn _) ik (d, ds) = do
      let entity = ikEntity ik
          key    = ikID     ik

      -- Use `void` to relabel the diffs and operations with () instead of
      -- the arbitrary, possibly unserialisable, labels.

      -- Extract the operations from the conflicting diffs.
      let ops = ds ^.. traverse . rejectedOperation . to toJSON
      let d' = d ^. patchDiff

      -- Record the merged diff in the database.
      [Only did] <- query conn diffQ (entity, key, not . null $ ops, encode d')

      -- Record conflicts in the database.
      void . executeMany conn opsQ . fmap (did,) $ ops
      return did
    where
      diffQ = "INSERT INTO retcon_diff (entity, id, is_conflict, content) VALUES (?, ?, ?, ?) RETURNING diff_id"
      opsQ = "INSERT INTO retcon_diff_conflicts (diff_id, content) VALUES (?, ?)"

  resolveDiffs (PGStore conn _) did =
      void $ execute conn sql (Only did)
    where
      sql = "UPDATE retcon_diff SET is_conflict = FALSE WHERE diff_id = ?"

  lookupDiff (PGStore conn _) diff_id = do
      let query' sql = query conn sql (Only diff_id)

      -- Load the merged diff.
      diff <- query' "SELECT entity, id, content FROM retcon_diff WHERE diff_id = ?"
      let diff' = fmap (\(entity, key, c) -> (entity,key,) <$> fromJSON c) diff

      -- Load the conflicting fragments.
      conflicts <- query conn "SELECT content FROM retcon_diff_conflicts WHERE diff_id = ?" (Only diff_id)
      let conflicts' = map fromSuccess $ filter isSuccess $ map (fromJSON . fromOnly) conflicts

      return $ case diff' of
          Success (entity,key,d):_ -> Just $ DiffResp entity key d conflicts'
          _ -> Nothing
    where
      _3 (_,_,z) = z
      fromSuccess (Success a) = a
      fromSuccess _ = error "fromSuccess: Cannot unwrap not-a-success."
      isSuccess (Success _) = True
      isSuccess _ = False

  -- | Lookup the list of conflicted 'Diff's with related information.
  lookupConflicts (PGStore conn _) = do
      diffs <- query_ conn diffS
      ops <- query_ conn opsS
      return $ fmap (match ops) diffs
    where
      -- Filter the operations which correspond to a diff and add them to the
      -- tuple.
      --
      -- TODO This is O(mn). I am embarrassing.
      -- thsutton
      match all_ops (doc, diff, diff_id) =
          let ops = fmap (\(_, op_id, op) -> (op_id, op))
                  . filter (\(op_diff_id,_,_) -> diff_id == op_diff_id)
                  $ all_ops
          in ConflictResp doc diff diff_id ops
      diffS = "SELECT CAST(doc.document AS TEXT), CAST(diff.content AS TEXT), diff.id "
          <> "FROM retcon_diff AS diff "
          <> "JOIN retcon_initial AS doc "
          <> "ON (diff.entity = doc.entity AND diff.id = doc.id) "
          <> "WHERE diff.is_conflict ORDER BY diff.id ASC"
      opsS = "SELECT op.diff_id, op.operation_id, CAST(op.content AS TEXT) "
          <> "FROM retcon_diff_conflicts AS op "
          <> "LEFT JOIN retcon_diff AS diff ON (op.diff_id = diff.diff_id) "
          <> "WHERE diff.is_conflict ORDER BY op.diff_id ASC"

  lookupDiffConflicts (PGStore conn _) ids =
      fmap parse <$> (query conn sql . Only . In $ ids)
    where
      sql = "SELECT diff_id, operation_id, content FROM retcon_diff_conflicts WHERE operation_id IN ?"
      parse (did, opid, op_json) =
          case fromJSON op_json of
              Success dop -> OpResp did opid dop
              Error e     -> error $
                 "Could not load diff operation: " <> show (did,opid) <> " " <> e

  lookupDiffIDs (PGStore conn _) ik = do
      r <- query conn "SELECT diff_id FROM retcon_diff WHERE entity = ? AND id = ?" ik
      return . fmap fromOnly $ r

  deleteDiff (PGStore conn _) diff_id = do
      let execute' sql = execute conn sql (Only diff_id)
      ops <- execute' "DELETE FROM retcon_diff_conflicts WHERE diff_id = ?"
      d <- execute' "DELETE FROM retcon_diff WHERE diff_id = ?"
      return . sum . fmap fromIntegral $ [ops, d]

  deleteDiffsWithKey (PGStore conn _) ik = do
      let execute' sql = execute conn sql ik
      d1 <- execute' "DELETE FROM retcon_diff_conflicts WHERE diff_id IN (SELECT diff_id FROM retcon_diff WHERE entity = ? AND id = ?)"
      d2 <- execute' "DELETE FROM retcon_notifications WHERE entity = ? AND id = ?"
      d3 <- execute' "DELETE FROM retcon_diff WHERE entity = ? AND id = ?"
      return . sum . fmap fromIntegral $ [d1, d2, d3]

  addWork (PGStore conn _) work = do
      let content = toJSON work
      void $ execute conn sql (Only content)
    where
      sql = "INSERT INTO retcon_workitems (content) VALUES (?)"

  getWork (PGStore conn _) = do
      res <- listToMaybe <$> query_ conn select
      case res of
         Just (work_id, work) -> case fromJSON work of
             Success x -> do void $ execute conn wait (Only work_id)
                             return (Just (work_id, x))
             _         -> return Nothing
         _ -> return Nothing
    where
      select = "SELECT id, content FROM retcon_workitems WHERE is_busy = FALSE ORDER BY id ASC LIMIT 1"
      wait   = "UPDATE retcon_workitems SET is_busy = TRUE WHERE id = ?"

  ungetWork (PGStore conn _) work_id =
      void $ execute conn sql (Only work_id)
    where
      sql = "UPDATE retcon_workitems SET is_busy = FALSE WHERE id = ?"

  completeWork (PGStore conn _) work_id =
      void $ execute conn sql (Only work_id)
    where
      sql = "DELETE FROM retcon_workitems WHERE id = ?"
