module Lib.Chord
  ( Chord (Chord),
    addMajorThird,
    addMajorSeventh,
    addMinorThird,
    addMinorSeventh,
    addNote,
    addPerfectFifth,
    addDiminishedFifth,
    addRoot,
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

addNote :: Note -> Chord -> Chord
addNote note chord = chord {notes = IntSet.insert (PitchClass.wrap note) $ notes chord}

addRoot :: Chord -> Chord
addRoot = addNote 0

addMinorThird :: Chord -> Chord
addMinorThird = addNote 3

addMajorThird :: Chord -> Chord
addMajorThird = addNote 4

addDiminishedFifth :: Chord -> Chord
addDiminishedFifth = addNote 6

addPerfectFifth :: Chord -> Chord
addPerfectFifth = addNote 7

addMinorSeventh :: Chord -> Chord
addMinorSeventh = addNote 10

addMajorSeventh :: Chord -> Chord
addMajorSeventh = addNote 11
