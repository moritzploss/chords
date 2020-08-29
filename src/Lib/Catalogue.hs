module Lib.Catalogue
  ( diminished,
    dominant,
    major,
    minor,
  )
where

import Lib.Chord
import Lib.Types (PitchClass)

diminished :: PitchClass -> Chord
diminished = addRoot . addMinorThird . addDiminishedFifth . base

dominant :: PitchClass -> Chord
dominant = major

major :: PitchClass -> Chord
major = addRoot . addMajorThird . addPerfectFifth . base

minor :: PitchClass -> Chord
minor = addRoot . addMinorThird . addPerfectFifth . base
