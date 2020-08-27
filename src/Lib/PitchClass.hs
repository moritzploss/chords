module Lib.PitchClass
  ( wrap,
    transpose,
    fromName,
  )
where

import Lib.Types (Interval, PitchClass)

wrap :: PitchClass -> PitchClass
wrap pitchClass = pitchClass `mod` 12

transpose :: Interval -> PitchClass -> PitchClass
transpose interval pitchClass = wrap $ interval + pitchClass

fromName :: String -> Maybe PitchClass
fromName string = case string of
  "C" -> Just 0
  "D" -> Just 2
  "E" -> Just 4
  "F" -> Just 5
  "G" -> Just 7
  "A" -> Just 9
  "B" -> Just 11
  _ -> Nothing
