--
-- Copyright © 2014-2015 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Data.Aeson
import           Data.Monoid
import           Test.Hspec
import           Test.HUnit

import           Retcon

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

fileSource :: DataSource
fileSource = DataSource
    { sourceEntity = "entity"
    , sourceName = "source"
    , sourceDescription = Nothing
    , commandCreate = "bash -c 'mkdir -p entity/source ; FOREIGN_KEY=$(mktemp -u -p \"entity/source\" -t XXXXXXXX.json) ; cat > $FOREIGN_KEY ; echo $FOREIGN_KEY;'"
    , commandRead   = "bash -c 'cat %fk 2>&1'"
    , commandUpdate = "bash -c 'mkdir -p entity/source; rm -f %fk; FOREIGN_KEY=$(mktemp -u -p \"entity/source\" -t XXXXXXXX.json); cat > $FOREIGN_KEY ; echo $FOREIGN_KEY;'"
    , commandDelete = "bash -c 'rm -f %fk 2>&1'"
    }

allDifferent :: (Eq a, Show a) => [a] -> Assertion
allDifferent [] = return ()
allDifferent (x:xs) = do
  when (x `elem` xs) . assertFailure $
    "allDifferent: found multiple occurances of " <> show x
  allDifferent xs

suite :: DataSource -> Spec
suite source =
    describe "DataSource" $ do
        it "can create and read out again" $ do
            let doc = Document "entity" "source" (object ["foo" .= ("bar" :: String)])
            fk' <- runDSMonad $ createDocument source doc
            fk <- case fk' of
                Left err -> assertFailure (show err) >>= undefined
                Right fk -> return fk
            res <- runDSMonad $ readDocument source fk
            case res of
                Left err -> assertFailure (show err)
                Right doc' -> assertBool "Read returned different object than created" (doc == doc')

        it "can create and read out again (multiple times)" $ do
            let docs = [Document "entity" "source" (object ["num" .= n]) | n <- [1..10::Int]]
            fks' <- mapM (runDSMonad . createDocument source) docs
            fks <- forM fks' $ \fk' -> case fk' of
                Left err -> assertFailure (show err) >>= undefined
                Right fk -> return fk
            allDifferent fks
            ress <- mapM (runDSMonad . readDocument source) fks
            forM_ (zip ress docs) $ \res -> case res of
                (Left err, _) -> assertFailure (show err)
                (Right doc', doc) -> assertBool "Read returned different object than created" (doc == doc')

        it "can create and delete" $ do
            let doc = Document "entity" "source" (object ["foo" .= ("bar" :: String)])
            fk' <- runDSMonad $ createDocument source doc
            fk <- case fk' of
                Left err -> assertFailure (show err) >>= undefined
                Right fk -> return fk
            res1 <- runDSMonad $ readDocument source fk
            case res1 of
                Left err -> assertFailure (show err)
                Right doc' -> assertBool "Read returned different object than created" (doc == doc')
            runDSMonad (deleteDocument source fk) `shouldReturn` Right ()
            res2 <- runDSMonad $ readDocument source fk
            case res2 of
                Left _ -> return ()
                Right _ -> assertFailure "delete not successful"

        it "can create and delete (multiple times)" $ do
            let docs = [Document "entity" "source" (object ["num" .= n]) | n <- [1..10::Int]]
            fks' <- mapM (runDSMonad . createDocument source ) docs
            fks <- forM fks' $ \fk' -> case fk' of
                Left err -> assertFailure (show err) >>= undefined
                Right fk -> return fk
            forM_ fks $ \fk -> do
                res1 <- runDSMonad $ readDocument source fk
                case res1 of
                    Left err -> assertFailure (show err)
                    Right _ -> return ()
                runDSMonad (deleteDocument source fk) `shouldReturn` Right ()
                res2 <- runDSMonad $ readDocument source fk
                case res2 of
                    Left _ -> return ()
                    Right _ -> assertFailure "delete not successful"

        it "can create and update" $ do
            let doc1 = Document "entity" "source" (object ["foo" .= ("bar" :: String)])
            fk1' <- runDSMonad $ createDocument source doc1
            fk1 <- case fk1' of
                Left err -> assertFailure (show err) >>= undefined
                Right fk -> return fk
            let doc2 = Document "entity" "source" (object ["foo" .= ("baz" :: String)])
            fk2' <- runDSMonad $ updateDocument source fk1 doc2
            fk2 <- case fk2' of
                Left err -> assertFailure (show err) >>= undefined
                Right fk -> return fk
            res1 <- runDSMonad $ readDocument source fk1
            case res1 of
                Left _ -> return ()
                Right _ -> assertBool "The old key should no longer be active" (fk1 /= fk2)
            res2 <- runDSMonad $ readDocument source fk2
            case res2 of
                Left err -> assertFailure (show err)
                Right doc' -> assertBool "Read returned different object than created" (doc2 == doc')

        it "can create and update (multiple times)" $ do
            let docs1 = [Document "entity" "source" (object ["num" .= n]) | n <- [1..10::Int]]
            fks1' <- mapM (runDSMonad . createDocument source ) docs1
            fks1 <- forM fks1' $ \fk' -> case fk' of
                Left err -> assertFailure (show err) >>= undefined
                Right fk -> return fk
            let docs2 = [Document "entity" "source" (object ["num" .= n]) | n <- [(101::Int)..]]
            fks2' <- mapM (runDSMonad . uncurry (updateDocument source)) (zip fks1 docs2)
            fks2 <- forM fks2' $ \fk' -> case fk' of
                Left err -> assertFailure (show err) >>= undefined
                Right fk -> return fk
            ress <- mapM (runDSMonad . readDocument source) fks2
            forM_ (zip ress docs2) $ \res -> case res of
                (Left err, _) -> assertFailure (show err)
                (Right doc', doc) -> assertBool "Read returned different object than created" (doc == doc')

main :: IO ()
main = hspec (suite fileSource)
