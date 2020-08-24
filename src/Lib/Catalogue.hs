module Lib.Catalogue
  ( dominantSeventh,
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

dominantSeventh :: PitchClass -> Chord
dominantSeventh = Chord.addMinorSeventh . major

majorSeventh :: PitchClass -> Chord
majorSeventh = Chord.addMajorSeventh . major

minorSeventh :: PitchClass -> Chord
minorSeventh = Chord.addMinorSeventh . minor
