module Lib.Compose (compose) where

import Lib.Catalogue (diminished, dominant, major, minor)
import Lib.Chord (Chord)
import qualified Lib.Chord as Chord
import Lib.Types (Factor, PitchClass)

type ChordType = String

composeMinor :: Factor -> PitchClass -> Maybe Chord
composeMinor factor root = case factor of
  "" -> Just $ minor root
  "7" -> Just $ (Chord.addMinorSeventh . minor) root
  _ -> Nothing

composeDiminished :: Factor -> PitchClass -> Maybe Chord
composeDiminished factor root = case factor of
  "" -> Just $ diminished root
  "7" -> Just $ (Chord.addDiminishedSeventh . diminished) root
  _ -> Nothing

composeMajor :: Factor -> PitchClass -> Maybe Chord
composeMajor factor root = case factor of
  "" -> Just $ major root
  "7" -> Just $ (Chord.addMajorSeventh . major) root
  _ -> Nothing

composeDominant :: Factor -> PitchClass -> Maybe Chord
composeDominant factor root = case factor of
  "" -> Just $ dominant root
  "7" -> Just $ (Chord.addMinorSeventh . major) root
  _ -> Nothing

compose :: ChordType -> Factor -> PitchClass -> Maybe Chord
compose chordType factor root
  | chordType `elem` ["", "dom", "dominant"] = composeType composeDominant
  | chordType `elem` ["M", "maj", "major"] = composeType composeMajor
  | chordType `elem` ["m", "min", "minor"] = composeType composeMinor
  | chordType `elem` ["o", "dim", "diminished"] = composeType composeDiminished
  | otherwise = Nothing
  where
    composeType chordFunc = chordFunc factor root
