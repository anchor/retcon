--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson.Diff (Operation (..))
import qualified Data.Aeson.Diff as D
import           Test.Hspec

import           Retcon.Diff

suite :: Spec
suite = do
    describe "ignoreConflicts merge policy" $ do
        let d1 = mkPatch
                 [ Ins [D.OKey "value"] "one"
                 , Ins [D.OKey "pet"] "cat"]

            d2 = mkPatch
                 [ Ins [D.OKey "value"] "two"
                 , Del [D.OKey "food"] ""]

            (accepted, rejected) = merge ignoreConflicts d1 d2

        it "should accept changes that do not conflict" $
            accepted `shouldBe` mkPatch [ Ins [D.OKey "pet"] "cat"
                                        , Del [D.OKey "food"] ""]

        it "should ignore all changes which conflict" $
            rejected `shouldBe` map mkRej [ Ins [D.OKey "value"] "one"
                                          , Ins [D.OKey "value"] "two" ]

    describe "rejectAll merge policy" $ do
        let d1    = mkPatch [Ins [D.OKey "value"] "one"]
            d2    = mkPatch [Ins [D.OKey "value"] "two"]
            (d,c) = merge rejectAll d1 d2

        it "should ignore all changes" $ do
            d `shouldBe` emptyPatch
            c `shouldBe` map mkRej [ Ins [D.OKey "value"] "one"
                                   , Ins [D.OKey "value"] "two" ]

    describe "acceptAll merge policy" $ do
        let d1    = mkPatch [Ins [D.OKey "value"] "one"]
            d2    = mkPatch [Ins [D.OKey "value"] "two"]
            (d,c) = merge acceptAll d1 d2

        it "should accept all changes" $ do
            d `shouldBe` mkPatch [ Ins [D.OKey "value"] "one"
                                 , Ins [D.OKey "value"] "two" ]
            c `shouldBe` []

    describe "trustOnly merge policy" $ do
        let d1    = Patch (Name "lord")
                  $ D.Patch [Ins [D.OKey "value"] "one"]
            d2    = Patch (Name "peasant")
                  $ D.Patch [Ins [D.OKey "value"] "two"]
            (d,c) = merge (trustOnlySource "lord") d1 d2

        it "should only accept changes from the specified source" $ do
            d `shouldBe` d1
            c `shouldBe` [ RejectedOp (Name "peasant") $ Ins [D.OKey "value"] "two" ]


mkPatch :: [Operation] -> Patch
mkPatch = Patch Unamed . D.Patch

mkRej :: Operation -> RejectedOp
mkRej = RejectedOp Unamed

main :: IO ()
main = hspec suite
