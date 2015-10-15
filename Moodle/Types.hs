module Moodle.Types (MoodleVal(..))
    where

import Data.Scientific (Scientific)


data MoodleVal = Number Scientific
               | Variable String
               | Op String MoodleVal MoodleVal
               | Function String [MoodleVal]
               deriving (Show)
