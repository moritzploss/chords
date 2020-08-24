module Lib.PitchClass
  ( wrap,
    transpose,
  )
where

import Lib.Types (Interval, PitchClass)

wrap :: PitchClass -> PitchClass
wrap pitchClass = pitchClass `mod` 12

transpose :: Interval -> PitchClass -> PitchClass
transpose interval pitchClass = wrap $ interval + pitchClass
