module Lib.Chord
  ( Chord (Chord),
    addRoot,
    addMinorNinth,
    addMajorNinth,
    addMinorThird,
    addMajorThird,
    addDiminishedFifth,
    addPerfectFifth,
    addMinorSeventh,
    addMajorSeventh,
    base,
    create,
    transpose,
    rebase,
  )
where

import Data.IntSet (IntSet (..))
import qualified Data.IntSet as IntSet
import qualified Lib.PitchClass as PitchClass
import Lib.Types (Interval, Note, PitchClass)

data Chord = Chord
  { pitchClass :: PitchClass,
    notes :: IntSet
  }
  deriving (Show, Eq)

create :: PitchClass -> [Note] -> Chord
create pitchClass notes =
  Chord
    { pitchClass = PitchClass.wrap pitchClass,
      notes = IntSet.fromList $ fmap PitchClass.wrap notes
    }

transpose :: Interval -> Chord -> Chord
transpose interval chord = chord {pitchClass = transposed}
  where
    transposed = PitchClass.transpose interval $ pitchClass chord

rebase :: Note -> Chord -> Chord
rebase note chord =
  chord
    { pitchClass = note,
      notes = IntSet.map (PitchClass.transpose interval) $ notes chord
    }
  where
    interval = pitchClass chord - note

base :: PitchClass -> Chord
base pitchClass = create pitchClass []

add :: Note -> Chord -> Chord
add note chord = chord {notes = IntSet.insert (PitchClass.wrap note) $ notes chord}

addRoot :: Chord -> Chord
addRoot = add 0

addMinorNinth :: Chord -> Chord
addMinorNinth = add 1

addMajorNinth :: Chord -> Chord
addMajorNinth = add 2

addMinorThird :: Chord -> Chord
addMinorThird = add 3

addMajorThird :: Chord -> Chord
addMajorThird = add 4

addDiminishedFifth :: Chord -> Chord
addDiminishedFifth = add 6

addPerfectFifth :: Chord -> Chord
addPerfectFifth = add 7

addMinorSixth :: Chord -> Chord
addMinorSixth = add 8

addMajorSixth :: Chord -> Chord
addMajorSixth = add 9

addMinorSeventh :: Chord -> Chord
addMinorSeventh = add 10

addMajorSeventh :: Chord -> Chord
addMajorSeventh = add 11
