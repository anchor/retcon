{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec

import Retcon.Diff
import Retcon.Document

import TestHelpers

-- | Hspec test suite for the 'Retcon.Diff' module.
suite :: Spec
suite = do
  describe "generating a diff" $ do
    it "should work with canned documents 01-*.json" $ do
      source <- testLoad' "01-diff-source.json"
      target <- testLoad' "01-diff-target.json"
      let patch = diff source target
          doc = applyDiff patch source
      doc `shouldBe` target

  describe "applying a diff" $ do
    it "should be idempotent with empty diffs" $ do
      doc <- testLoad' "01-diff-source.json"
      let doc' = applyDiff emptyDiff doc
      doc' `shouldBe` doc

    it "applyDiff (diff empty doc) empty = doc" $ do
      doc <- testLoad' "01-diff-source.json"
      let patch = diff emptyDocument doc
      let doc' = applyDiff patch emptyDocument
      doc' `shouldBe` doc

-- | Run the test suite.
main :: IO ()
main = hspec suite
