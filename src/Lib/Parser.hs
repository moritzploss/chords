{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib.Parser (parse, toMatch) where

import qualified Lib.Catalogue as Cat
import Lib.Chord (Chord, transpose)
import Lib.Types (PitchClass)
import Text.Regex.PCRE.Heavy

base :: String -> PitchClass
base "C" = 0
base "D" = 2
base "E" = 4
base "F" = 5
base "G" = 7
base "A" = 9
base "B" = 11

regex :: Regex
regex = [re|(#|b){0,1}([A,B,C,D,E,F,G]{1})(maj|dom|m{0,1})([6,7,9]{0,1})|]

toMatch :: String -> Maybe [String]
toMatch chord = case scan regex chord of
  [("", _)] -> Nothing
  [(_, matches)] -> Just matches

toChord :: [String] -> Maybe Chord
toChord matches = case matches of
  ("#" : tail) -> transposeTail 1 tail
  ("b" : tail) -> transposeTail (-1) tail
  ["", root, _, "7"] -> Just $ Cat.majorSeventh $ base root
  [_, root, _, _] -> Just $ Cat.major $ base root
  where
    transposeTail i tail = fmap (transpose i) $ parse $ concat tail

parse :: String -> Maybe Chord
parse chord = toMatch chord >>= toChord
