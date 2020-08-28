{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib.Parse (parse, match, toChord) where

import qualified Lib.Catalogue as Catalogue
import Lib.Chord (Chord)
import qualified Lib.Chord as Chord
import qualified Lib.Compose as Compose
import qualified Lib.PitchClass as Pitch
import Lib.Types (ChordPattern, Interval, Note, NoteName, PitchClass)
import qualified Text.Regex.PCRE.Heavy as Regex

type RegexMatch = [String]

match :: ChordPattern -> Maybe RegexMatch
match pattern = case Regex.scan regex pattern of
  [] -> Nothing
  [("", _)] -> Nothing
  [(_, match)] -> Just match
  where
    regex = [Regex.re|^([A-G]{1})(#|b)?(M|major|maj|o|diminished|dim|minor|min|m|dominant|dom)?(6|7|9?)(?:\/([A-G]))?$|]

rebase :: NoteName -> ChordPattern -> Maybe Chord
rebase name pattern = Chord.rebase <$> Pitch.fromName name <*> (parse pattern)

transpose :: Interval -> RegexMatch -> Maybe Chord
transpose interval = fmap (Chord.transpose interval) . parse . concat

toChord :: RegexMatch -> Maybe Chord
toChord matches = case matches of
  match@[_, _, _, _, root] -> rebase root $ concat $ init match
  (root : "#" : tail) -> transpose 1 (root : tail)
  (root : "b" : tail) -> transpose (-1) (root : tail)
  [root, "", chordType, added] -> Pitch.fromName root >>= Compose.compose chordType added
  _ -> Nothing

parse :: ChordPattern -> Maybe Chord
parse pattern = match pattern >>= toChord
