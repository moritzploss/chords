module Spec.Catalogue (spec) where

import Control.Applicative
import Lib.Catalogue
import qualified Lib.Chord as Chord
import Test.Hspec

spec :: Spec
spec = do
  describe "Chord Catalogue" $ do
    describe "compose" $ do
      it "minor chord" $
        minor 2 `shouldBe` Chord.create 2 [0, 3, 7]
      it "major chord" $
        major 9 `shouldBe` Chord.create 9 [0, 4, 7]
      it "dominant seventh chord" $
        dominantSeventh 3 `shouldBe` Chord.create 3 [0, 4, 7, 10]
      it "minor seventh chord" $
        minorSeventh 8 `shouldBe` Chord.create 8 [0, 3, 7, 10]
      it "major seventh chord" $
        majorSeventh 10 `shouldBe` Chord.create 10 [0, 4, 7, 11]
