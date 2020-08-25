module Spec.Parser (spec) where

import Control.Applicative
import Data.Maybe (fromJust)
import qualified Lib.Chord as Chord
import Lib.Parser (parse, toMatch)
import Test.Hspec

spec :: Spec
spec = do
  describe "Chord Parser" $ do
    describe "match" $ do
      it "major chord" $
        fromJust (toMatch "C") `shouldBe` ["", "C", "", ""]
      it "sharp chord" $
        fromJust (toMatch "#C") `shouldBe` ["#", "C", "", ""]
      it "flat chord" $
        fromJust (toMatch "bC") `shouldBe` ["b", "C", "", ""]

    describe "parse" $ do
      it "major chord" $
        fromJust (parse "C") `shouldBe` Chord.create 0 [0, 4, 7]
      it "sharp chord" $
        fromJust (parse "#C") `shouldBe` Chord.create 1 [0, 4, 7]
      it "flat chord" $
        fromJust (parse "bC") `shouldBe` Chord.create 11 [0, 4, 7]
