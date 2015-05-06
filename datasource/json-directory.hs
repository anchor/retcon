{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- * A simple retcond datasource backed by JSON files in directories.
--
module Main where

import           Control.Exception
import           Data.Aeson
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as BL
import           Data.Monoid
import qualified Data.Text             as T
import           Options.Applicative
import           System.Directory
import           System.Exit
import           System.FilePath.Posix
import           System.Posix.Files
import           System.Random

import           Retcon.DataSource
import           Retcon.Document
import           Retcon.Identifier


data JSONOpts = JSONOpts
  { baseDir :: FilePath
  , cmmnd   :: JSONCommand }

data JSONCommand
  = Create
  | Update ForeignID
  | Read   ForeignID
  | Delete ForeignID

pOpts :: Parser JSONOpts
pOpts
  =   JSONOpts
  <$> strArgument (metavar "BASE")
  <*> pCommand

pCommand :: Parser JSONCommand
pCommand
  = subparser
  (  command "create" (info pCreate (progDesc "Create a new unique JSON filename"))
  <> command "update" (info pUpdate (progDesc "Writes a document to the given JSON directory"))
  <> command "read"   (info pRead   (progDesc "Reads a document in the given JSON directory"))
  <> command "delete" (info pDelete (progDesc "Deletes a document in the given JSON directory")))
  where pCreate = pure Create
        pUpdate = Update <$> pFK
        pRead   = Read   <$> pFK
        pDelete = Delete <$> pFK
        pFK     = T.pack <$> strArgument (metavar "KEY")

main :: IO ()
main = do
  JSONOpts{..} <- execParser toplevel

  createDirectoryIfMissing True baseDir
  exit <- handle catchAll (run baseDir cmmnd)
  exitWith exit
  where
    toplevel = info pOpts mempty

    run :: FilePath -> JSONCommand -> IO ExitCode
    run d c = const (return ExitSuccess) =<< case c of
      Create     -> jsonCreate  d
      Read   f   -> jsonRead    (mkJSONFilename d f)
      Update f   -> jsonWrite   (mkJSONFilename d f)
      Delete f   -> jsonDelete  (mkJSONFilename d f)

    catchAll :: SomeException -> IO ExitCode
    catchAll e = do
      print e
      return (ExitFailure 1)

--------------------------------------------------------------------------------

-- | Creates a new JSON file under the path specified.
--   Prints the filename to `stdout`.
--
jsonCreate :: FilePath -> IO ()
jsonCreate base = do
  k      <-  T.pack . take 64 . randomRs ('a', 'z')
         <$> newStdGen
  let p  = mkJSONFilename base k
  exists <- fileExist p
  if      exists
  then    jsonCreate base
  else do putStrLn (T.unpack k)
          jsonWrite p

-- | Read a 'Document' from the given JSON directory.
--   Prints the content to `stdout`.
--
jsonRead :: FilePath -> IO ()
jsonRead path = do
  doc <- loadDocument path
  BL.putStr (encode doc)

-- | Update a 'Document' with input from stdin.
--
--   Assuming the input is already in JSON and makes no attempt to check. It is
--   retcond's repsonsiblity.
--
jsonWrite :: FilePath -> IO ()
jsonWrite path = do
  doc <- B.getContents
  B.writeFile path doc

-- | Unlink the underlying JSON file corresponding to the directory and 'ForeignKey'.
jsonDelete :: FilePath -> IO ()
jsonDelete = removeFile

-- | Construct a path to a JSON file.
mkJSONFilename :: FilePath -> ForeignID -> FilePath
mkJSONFilename dir f
  = dir </> T.unpack f <> ".json"

-- | Decode a 'Document' from the JSON file at the given 'FilePath'
loadDocument :: FilePath -> IO Document
loadDocument fp
  =  eitherDecode <$> BL.readFile fp
 >>= either (throw . ForeignError 1 . T.pack) return
