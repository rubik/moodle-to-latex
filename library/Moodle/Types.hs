module Moodle.Types (MoodleVal(..))
    where

import Data.Scientific (Scientific)


-- | This type represent every possible value in the Moodle language
data MoodleVal = Number Scientific             -- ^ A real number
               | Variable String               -- ^ A variable
               | Op String MoodleVal MoodleVal -- ^ A binary operator
               | Function String [MoodleVal]   -- ^ A function call
               deriving (Show, Eq)
