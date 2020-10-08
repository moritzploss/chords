module Spec.CatalogueSpec (spec) where

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
      it "dominant chord" $
        dominant 4 `shouldBe` Chord.create 4 [0, 4, 7]
      it "diminished chord" $
        diminished 6 `shouldBe` Chord.create 6 [0, 3, 6]
