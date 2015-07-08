{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Retcon.Store.PostgreSQL
     ( PGStore(..)
     , StoreOpts(..)
     ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens               hiding (op)
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Diff            ()
import qualified Data.Aeson.Diff            as D
import           Data.ByteString            (ByteString)
import qualified Data.List                  as L
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Database.PostgreSQL.Simple

import           Retcon.Diff                hiding (diff)
import           Retcon.Identifier
import           Retcon.Network.Ekg
import           Retcon.Store.Base          hiding (ops)

data PGStore = PGStore
    { pgconn    :: !Connection
    , pgconnstr :: !ByteString
    }

-- | Concatenate SQL queries together.
--
--   If you use this function you are probably a terrible person.
sqlConcat :: IsString x => [String] -> x
sqlConcat = fromString . L.intercalate ";"

onlySuccess :: FromJSON a => [Only Value] -> [a]
onlySuccess = map fromSuccess . filter isSuccess . map (fromJSON . fromOnly)

fromSuccess :: Result t -> t
fromSuccess (Success a) = a
fromSuccess _           = error "fromSuccess: Cannot unwrap not-a-success."

isSuccess :: Result a -> Bool
isSuccess (Success _)   = True
isSuccess _             = False

-- | Update ekg values for everything
updateEkg :: Connection -> IO ()
updateEkg conn = do
    updateIKCounts      conn
    updateFKCounts      conn
    updateConflicts     conn
    updateNotifications conn

-- | Update ekg values for internal keys
updateIKCounts :: Connection -> IO ()
updateIKCounts conn = do
    res <- query_ conn "SELECT COUNT(DISTINCT id), entity FROM retcon GROUP BY entity"
    forM_ res $ uncurry setEntityKeys . second EntityName

-- | Update ekg values for foreign keys
updateFKCounts :: Connection -> IO ()
updateFKCounts conn = do
    res <- query_ conn "SELECT COUNT(DISTINCT id), entity, source FROM retcon_fk GROUP BY source, entity"
    forM_ res $ \(c, e, s) -> setSourceKeys c (EntityName e) (SourceName s)

-- | Update ekg values for conflicts
updateConflicts :: Connection -> IO ()
updateConflicts conn = do
    res <- query_ conn "SELECT COUNT(DISTINCT diff_id), entity FROM retcon_diff WHERE is_conflict GROUP BY entity"
    forM_ res $ \(c, e) -> setConflicts c (EntityName e)

-- | Update ekg values for notifications
updateNotifications :: Connection -> IO ()
updateNotifications conn = do
    res <- query_ conn "SELECT COUNT(DISTINCT id), entity FROM retcon_notifications GROUP BY entity"
    let total = sum $ map fst res
    forM_ res $ \(c, e) -> setEntityNotifications c (EntityName e)
    setServerNotifications total

-- | Persistent PostgreSQL-backed data storage.
instance Store PGStore where
  newtype StoreOpts PGStore = PGOpts { connstr :: ByteString }

  initBackend (connstr -> str) = do
      conn <- connectPostgreSQL str
      -- Upon new connection initialise all metrics
      updateEkg conn
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
          (Only key:_) -> do
              updateIKCounts conn
              return $ InternalKey entity key

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
      updateFKCounts conn
      updateIKCounts conn
      return . sum . fmap fromIntegral $ [d1, d2, d3, d4]

  recordForeignKey (PGStore conn _) ik fk = do
      let entity = fkEntity fk
          source = fkSource fk
          fid    = fkID     fk
          iid    = ikID     ik
          values = (entity, iid, source, entity, iid, source, fid)

      -- TODO: This should be a real transaction or UPSERT or something.
      let sql = "DELETE FROM retcon_fk WHERE entity = ? AND id = ? AND source = ?; INSERT INTO retcon_fk (entity, id, source, fk) VALUES (?, ?, ?, ?)"
      updateFKCounts conn
      void $ execute conn sql values

  deleteForeignKey (PGStore conn _) fk = do
      let sql = "DELETE FROM retcon_fk WHERE entity = ? AND source = ? AND fk = ?"
      d <- execute conn sql fk
      updateFKCounts conn
      return $ fromIntegral  d

  deleteForeignKeysWithInternal (PGStore conn _) ik = do
      let sql = "DELETE FROM retcon_fk WHERE entity = ? AND id = ?"
      d <- execute conn sql ik
      updateFKCounts conn
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
      updateConflicts conn
      return did
    where
      diffQ = "INSERT INTO retcon_diff (entity, id, is_conflict, content) VALUES (?, ?, ?, ?) RETURNING diff_id"
      opsQ = "INSERT INTO retcon_diff_conflicts (diff_id, content) VALUES (?, ?)"

  resolveDiffs (PGStore conn _) did = do
      _ <- execute conn "DELETE FROM retcon_diff_conflicts WHERE diff_id = ?" (Only did)
      _ <- execute conn "UPDATE retcon_diff SET is_conflict = FALSE WHERE diff_id = ?" (Only did)
      updateConflicts conn

  reduceDiff (PGStore conn _) patchID resolved = do
      -- Change the corresponding conflicts
      conops <- getOps
      forM_ (map Only $ filterOps conops)
           $ execute conn "DELETE FROM retcon_diff_conflicts WHERE operation_id = ?"
      updateConflicts conn
    where
      getOps :: IO [(OpID, Value)]
      getOps = query conn "SELECT operation_id, content FROM retcon_diff_conflicts WHERE diff_id = ?" (Only patchID)

      filterOps = map fst
                . filter (flip elem resolved . D.changePath . snd)
                . map (fmap fromSuccess)
                . filter (isSuccess . snd)
                . map (fmap fromJSON)

  lookupDiff (PGStore conn _) diff_id = do
      -- Load the merged diff.
      diff <- query conn "SELECT entity, id, content FROM retcon_diff WHERE diff_id = ?" (Only diff_id)
      let diff' = fmap (\(entity, key, c) -> (entity,key,) <$> fromJSON c) diff

      -- Load the conflicting fragments.
      conflicts <- query conn "SELECT content FROM retcon_diff_conflicts WHERE diff_id = ?" (Only diff_id)

      return $ case diff' of
          Success (entity,key,d):_ -> Just $ DiffResp entity key d (onlySuccess conflicts)
          _                        -> Nothing

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
      updateConflicts conn
      return . sum . fmap fromIntegral $ [ops, d]

  deleteDiffsWithKey (PGStore conn _) ik = do
      let execute' sql = execute conn sql ik
      d1 <- execute' "DELETE FROM retcon_diff_conflicts WHERE diff_id IN (SELECT diff_id FROM retcon_diff WHERE entity = ? AND id = ?)"
      d2 <- execute' "DELETE FROM retcon_notifications WHERE entity = ? AND id = ?"
      d3 <- execute' "DELETE FROM retcon_diff WHERE entity = ? AND id = ?"
      updateNotifications conn
      updateConflicts conn
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
