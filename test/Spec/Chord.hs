module Spec.Chord (spec) where

import Control.Applicative
import Lib.Chord (create, transpose)
import Test.Hspec

spec :: Spec
spec = do
  describe "Chords" $ do
    describe "create" $ do
      it "with positive pitchclass wrapping" $
        create 15 [] `shouldBe` create 3 []
      it "with positive note wrapping" $
        create 3 [15, 16, 5] `shouldBe` create 3 [3, 4, 5]
      it "with negative pitchclass wrapping" $
        create (-5) [] `shouldBe` create 7 []
      it "with negative note wrapping" $
        create 3 [-5, -6, 5] `shouldBe` create 3 [7, 6, 5]
      it "ignore duplicate notes" $
        create 3 [1, 1, 2] `shouldBe` create 3 [1, 2]

    describe "transpose" $ do
      it "positive without wrapping" $
        transpose 2 (create 0 notes) `shouldBe` create 2 notes
      it "positive with wrapping" $
        transpose 14 (create 0 notes) `shouldBe` create 2 notes
      it "negative without wrapping" $
        transpose (-3) (create 5 notes) `shouldBe` create 2 notes
      it "negative with wrapping" $
        transpose (-15) (create 5 notes) `shouldBe` create 2 notes
  where
    notes = [1, 2, 3]
