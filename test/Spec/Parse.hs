module Spec.Parse (spec) where

import Control.Applicative
import Data.Maybe (fromJust, isNothing)
import qualified Lib.Chord as Chord
import Lib.Parse (match, parse)
import Test.Hspec

spec :: Spec
spec = do
  describe "Chord Parser" $ do
    describe "match" $ do
      it "major chord" $
        fromJust (match "C") `shouldBe` ["C", "", ""]
      it "sharp chord" $
        fromJust (match "F#/A") `shouldBe` ["F#", "", "", "A"]
      it "flat chord" $
        fromJust (match "Dbmaj9") `shouldBe` ["Db", "maj", "9"]
      it "slash chord" $
        fromJust (match "Dbmaj9/F") `shouldBe` ["Db", "maj", "9", "F"]
      it "all together now" $
        fromJust (match "Dbo9/G#") `shouldBe` ["Db", "o", "9", "G#"]

    describe "parse mixed" $ do
      it "major chord" $
        fromJust (parse "C") `shouldBe` Chord.create 0 [0, 4, 7]
      it "sharp chord" $
        fromJust (parse "C#") `shouldBe` Chord.create 1 [0, 4, 7]
      it "flat chord" $
        fromJust (parse "Cb") `shouldBe` Chord.create 11 [0, 4, 7]
      it "slash chord" $
        fromJust (parse "D/E") `shouldBe` Chord.create 4 [2, 5, 10]

    describe "parse failure" $ do
      it "unknown note" $
        isNothing (parse "H") `shouldBe` True
      it "sharp in wrong position" $
        isNothing (parse "#C") `shouldBe` True
      it "sharp and flat" $
        isNothing (parse "C#b") `shouldBe` True
      it "unknown chord type" $
        isNothing (parse "Cmo") `shouldBe` True

    describe "major chords" $ do
      it "major chord with M" $
        fromJust (parse "CM") `shouldBe` Chord.create 0 [0, 4, 7]
      it "major chord with maj" $
        fromJust (parse "Cmaj") `shouldBe` Chord.create 0 [0, 4, 7]
      it "major chord with major" $
        fromJust (parse "Cmajor") `shouldBe` Chord.create 0 [0, 4, 7]
      it "major seventh chord" $
        fromJust (parse "CM7") `shouldBe` Chord.create 0 [0, 4, 7, 11]

    describe "minor chords" $ do
      it "minor chord with m" $
        fromJust (parse "Cm") `shouldBe` Chord.create 0 [0, 3, 7]
      it "minor chord with min" $
        fromJust (parse "Cmin") `shouldBe` Chord.create 0 [0, 3, 7]
      it "minor chord with minor" $
        fromJust (parse "Cminor") `shouldBe` Chord.create 0 [0, 3, 7]
      it "minor seventh chord" $
        fromJust (parse "Cm7") `shouldBe` Chord.create 0 [0, 3, 7, 10]

    describe "dominant chords" $ do
      it "dominant chord" $
        fromJust (parse "C") `shouldBe` Chord.create 0 [0, 4, 7]
      it "dominant chord with dom" $
        fromJust (parse "Cdom") `shouldBe` Chord.create 0 [0, 4, 7]
      it "dominant chord with dominant" $
        fromJust (parse "Cdominant") `shouldBe` Chord.create 0 [0, 4, 7]
      it "dominant seventh chord" $
        fromJust (parse "C7") `shouldBe` Chord.create 0 [0, 4, 7, 10]

    describe "diminished chords" $ do
      it "diminished chord with o" $
        fromJust (parse "Co") `shouldBe` Chord.create 0 [0, 3, 6]
      it "diminished chord with dim" $
        fromJust (parse "Cdim") `shouldBe` Chord.create 0 [0, 3, 6]
      it "diminished chord with diminished" $
        fromJust (parse "Cdiminished") `shouldBe` Chord.create 0 [0, 3, 6]
      it "diminished seventh chord" $
        fromJust (parse "Co7") `shouldBe` Chord.create 0 [0, 3, 6, 9]
