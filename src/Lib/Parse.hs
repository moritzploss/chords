{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib.Parse (parse, match, toChord) where

import qualified Data.ByteString.Char8 as Char8
import Lib.Chord (Chord)
import qualified Lib.Chord as Chord
import qualified Lib.Compose as Compose
import qualified Lib.PitchClass as Pitch
import Lib.Types (ChordPattern, Interval, NoteName)
import qualified Text.Regex.PCRE.Heavy as Regex
import qualified Text.Regex.PCRE.Light as PCRE

type RegexMatch = [String]

asRegex :: String -> PCRE.Regex
asRegex string = PCRE.compile (Char8.pack string) []

match :: ChordPattern -> Maybe RegexMatch
match pattern = case Regex.scan regex pattern of
  [] -> Nothing
  [("", _)] -> Nothing
  [(_, match)] -> Just match
  where
    regex = asRegex $ "^" ++ root ++ chordType ++ addedFactor ++ slash ++ "$"
    root = "([A-G]{1}(?:#|b)?)"
    chordType = "(M|major|maj|o|diminished|dim|minor|min|m|dominant|dom)?"
    addedFactor = "(6|7|9?)"
    slash = "(?:/" ++ root ++ ")?"

rebase :: NoteName -> ChordPattern -> Maybe Chord
rebase name pattern = Chord.rebase <$> Pitch.fromName name <*> (parse pattern)

transpose :: Interval -> RegexMatch -> Maybe Chord
transpose interval = fmap (Chord.transpose interval) . parse . concat

toChord :: RegexMatch -> Maybe Chord
toChord matches = case matches of
  match@[_, _, _, root] -> rebase root $ concat $ init match
  [root, chordType, factor] -> Pitch.fromName root >>= Compose.compose chordType factor
  _ -> Nothing

parse :: ChordPattern -> Maybe Chord
parse pattern = match pattern >>= toChord
