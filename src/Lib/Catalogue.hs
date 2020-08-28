module Lib.Catalogue
  ( diminished,
    dominant,
    dominantSeventh,
    major,
    majorSeventh,
    minor,
    minorSeventh,
  )
where

import Lib.Chord (Chord)
import qualified Lib.Chord as Chord
import Lib.Types (PitchClass)

minor :: PitchClass -> Chord
minor = Chord.addRoot . Chord.addMinorThird . Chord.addPerfectFifth . Chord.base

major :: PitchClass -> Chord
major = Chord.addRoot . Chord.addMajorThird . Chord.addPerfectFifth . Chord.base

dominant :: PitchClass -> Chord
dominant = major

dominantSeventh :: PitchClass -> Chord
dominantSeventh = Chord.addMinorSeventh . dominant

diminished :: PitchClass -> Chord
diminished = Chord.addRoot . Chord.addMinorThird . Chord.addDiminishedFifth . Chord.base

majorSeventh :: PitchClass -> Chord
majorSeventh = Chord.addMajorSeventh . major

minorSeventh :: PitchClass -> Chord
minorSeventh = Chord.addMinorSeventh . minor
