{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib.Parse (parse, asMatch, toChord) where

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

asMatch :: ChordPattern -> Maybe RegexMatch
asMatch pattern = case Regex.scan regex pattern of
  [] -> Nothing
  [("", _)] -> Nothing
  [(_, match)] -> Just match
  where
    regex = asRegex $ "^" ++ root ++ chordType ++ addedFactor ++ slash ++ "$"
    root = "([A-G]{1}(?:#|b)?)"
    chordType = "(M|major|maj|o|diminished|dim|minor|min|m|dominant|dom)?"
    addedFactor = "(6|7|9?)"
    slash = "(?:/" ++ root ++ ")?"

rebase :: NoteName -> Maybe Chord -> Maybe Chord
rebase name chord = Chord.rebase <$> Pitch.fromName name <*> chord

toChord :: RegexMatch -> Maybe Chord
toChord match = case match of
  [_, _, _, slash] -> rebase slash $ compose $ init match
  [_, _, _] -> compose match
  _ -> Nothing
  where
    compose [root, chordType, factor] = Pitch.fromName root >>= Compose.compose chordType factor

parse :: ChordPattern -> Maybe Chord
parse pattern = asMatch pattern >>= toChord
