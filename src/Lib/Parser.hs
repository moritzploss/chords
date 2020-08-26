{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib.Parser (parse, asMatch, asChord) where

import qualified Lib.Catalogue as Cat
import Lib.Chord (Chord (Chord))
import qualified Lib.Chord as Chord
import qualified Lib.PitchClass as Pitch
import Lib.Types (Note, PitchClass)
import qualified Text.Regex.PCRE.Heavy as RegEx

asMatch :: String -> Maybe [String]
asMatch chord = case RegEx.scan regex chord !! 0 of
  ("", _) -> Nothing
  (_, match) -> Just match
  where
    regex = [RegEx.re|([A-G]{1})(#|b?)(maj|dom|m?)([6,7,9]?)(?:\/([A-G]))?|]

rebase :: String -> String -> Maybe Chord
rebase root chord = Chord.rebase <$> Pitch.fromString root <*> (parse chord)

transpose :: Note -> [String] -> Maybe Chord
transpose interval = fmap (Chord.transpose interval) . parse . concat

asChord :: [String] -> Maybe Chord
asChord matches = case matches of
  (root : "#" : tail) -> transpose 1 (root : tail)
  (root : "b" : tail) -> transpose (-1) (root : tail)
  match@[_, _, _, _, root] -> rebase root $ concat $ init match
  [root, _, "7", _] -> Cat.majorSeventh <$> Pitch.fromString root
  [root, _, _, _] -> Cat.major <$> Pitch.fromString root

parse :: String -> Maybe Chord
parse chord = asMatch chord >>= asChord
