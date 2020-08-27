{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib.Parser (parse, asMatch, asChord) where

import qualified Lib.Catalogue as Catalogue
import Lib.Chord (Chord (Chord))
import qualified Lib.Chord as Chord
import qualified Lib.PitchClass as Pitch
import Lib.Types (Interval, Note, NoteName, PitchClass)
import qualified Text.Regex.PCRE.Heavy as RegEx

type RegExMatch = [String]

type ChordPattern = String

asMatch :: ChordPattern -> Maybe RegExMatch
asMatch chord = case RegEx.scan regex chord !! 0 of
  ("", _) -> Nothing
  (_, match) -> Just match
  where
    regex = [RegEx.re|([A-G]{1})(#|b?)(M|maj|dim|min|m?)([6,7,9]?)(?:\/([A-G]))?|]

minor :: String -> PitchClass -> Maybe Chord
minor added root = case added of
  "" -> Just $ Catalogue.minor root
  "7" -> Just $ Catalogue.minorSeventh root
  _ -> Nothing

major :: String -> PitchClass -> Maybe Chord
major added root = case added of
  "" -> Just $ Catalogue.major root
  "7" -> Just $ Catalogue.majorSeventh root
  _ -> Nothing

dominant :: String -> PitchClass -> Maybe Chord
dominant added root = case added of
  "" -> Just $ Catalogue.dominant root
  "7" -> Just $ Catalogue.dominantSeventh root
  _ -> Nothing

compose :: String -> String -> PitchClass -> Maybe Chord
compose chordType added root
  | chordType `elem` ["", "dom", "dominant"] = composeType dominant
  | chordType `elem` ["M", "maj", "major"] = composeType major
  | chordType `elem` ["m", "min", "minor"] = composeType minor
  | otherwise = Nothing
  where
    composeType chordFunc = chordFunc added root

rebase :: NoteName -> ChordPattern -> Maybe Chord
rebase name pattern = Chord.rebase <$> Pitch.fromName name <*> (parse pattern)

transpose :: Interval -> RegExMatch -> Maybe Chord
transpose interval = fmap (Chord.transpose interval) . parse . concat

asChord :: RegExMatch -> Maybe Chord
asChord matches = case matches of
  match@[_, _, _, _, root] -> rebase root $ concat $ init match
  (root : "#" : tail) -> transpose 1 (root : tail)
  (root : "b" : tail) -> transpose (-1) (root : tail)
  [root, "", chordType, added] -> Pitch.fromName root >>= compose chordType added
  _ -> Nothing

parse :: ChordPattern -> Maybe Chord
parse pattern = asMatch pattern >>= asChord
