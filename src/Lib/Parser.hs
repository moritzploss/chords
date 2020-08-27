{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib.Parser (parse, asMatch, asChord) where

import qualified Lib.Catalogue as Cat
import Lib.Chord (Chord (Chord))
import qualified Lib.Chord as Chord
import qualified Lib.PitchClass as Pitch
import Lib.Types (Note, PitchClass)
import qualified Text.Regex.PCRE.Heavy as RegEx

type RegExMatch = [String]

type ChordPattern = String

asMatch :: ChordPattern -> Maybe RegExMatch
asMatch chord = case RegEx.scan regex chord !! 0 of
  ("", _) -> Nothing
  (_, match) -> Just match
  where
    regex = [RegEx.re|([A-G]{1})(#|b?)(M|maj|dim|min|m?)([6,7,9]?)(?:\/([A-G]))?|]

rebase :: String -> ChordPattern -> Maybe Chord
rebase root pattern = Chord.rebase <$> Pitch.fromString root <*> (parse pattern)

transpose :: Note -> RegExMatch -> Maybe Chord
transpose interval = fmap (Chord.transpose interval) . parse . concat

asMinorChord :: String -> PitchClass -> Maybe Chord
asMinorChord added root = case added of
  "" -> Just $ Cat.minor root
  "7" -> Just $ Cat.minorSeventh root
  _ -> Nothing

asChord :: RegExMatch -> Maybe Chord
asChord matches = case matches of
  match@[_, _, _, _, root] -> rebase root $ concat $ init match
  (root : "#" : tail) -> transpose 1 (root : tail)
  (root : "b" : tail) -> transpose (-1) (root : tail)
  [root, "", "", _] -> Cat.major <$> Pitch.fromString root
  [root, "", "maj", _] -> Cat.major <$> Pitch.fromString root
  [root, "", "m", add] -> Pitch.fromString root >>= asMinorChord add
  _ -> Nothing

parse :: ChordPattern -> Maybe Chord
parse pattern = asMatch pattern >>= asChord
