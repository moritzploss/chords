module Spec.Parser (spec) where

import Control.Applicative
import Data.Maybe (fromJust)
import qualified Lib.Chord as Chord
import Lib.Parser (asMatch, parse)
import Test.Hspec

spec :: Spec
spec = do
  describe "Chord Parser" $ do
    describe "match" $ do
      it "major chord" $
        fromJust (asMatch "C") `shouldBe` ["C", "", "", ""]
      it "sharp chord" $
        fromJust (asMatch "F#/A") `shouldBe` ["F", "#", "", "", "A"]
      it "flat chord" $
        fromJust (asMatch "Dbmaj9") `shouldBe` ["D", "b", "maj", "9"]
      it "all together now" $
        fromJust (asMatch "Dbmaj9/G") `shouldBe` ["D", "b", "maj", "9", "G"]

    describe "parse" $ do
      it "major chord" $
        fromJust (parse "C") `shouldBe` Chord.create 0 [0, 4, 7]
      it "sharp chord" $
        fromJust (parse "C#") `shouldBe` Chord.create 1 [0, 4, 7]
      it "flat chord" $
        fromJust (parse "Cb") `shouldBe` Chord.create 11 [0, 4, 7]
      it "slash chord" $
        fromJust (parse "D/E") `shouldBe` Chord.create 4 [2, 5, 10]
