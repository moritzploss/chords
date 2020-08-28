module Lib.Compose (compose) where

import qualified Lib.Catalogue as Catalogue
import Lib.Chord (Chord)
import Lib.Types (Added, PitchClass)

type ChordType = String

minor :: Added -> PitchClass -> Maybe Chord
minor added root = case added of
  "" -> Just $ Catalogue.minor root
  "7" -> Just $ Catalogue.minorSeventh root
  _ -> Nothing

major :: Added -> PitchClass -> Maybe Chord
major added root = case added of
  "" -> Just $ Catalogue.major root
  "7" -> Just $ Catalogue.majorSeventh root
  _ -> Nothing

dominant :: Added -> PitchClass -> Maybe Chord
dominant added root = case added of
  "" -> Just $ Catalogue.dominant root
  "7" -> Just $ Catalogue.dominantSeventh root
  _ -> Nothing

compose :: ChordType -> Added -> PitchClass -> Maybe Chord
compose chordType added root
  | chordType `elem` ["", "dom", "dominant"] = composeType dominant
  | chordType `elem` ["M", "maj", "major"] = composeType major
  | chordType `elem` ["m", "min", "minor"] = composeType minor
  | otherwise = Nothing
  where
    composeType chordFunc = chordFunc added root
