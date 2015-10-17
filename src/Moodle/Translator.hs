module Moodle.Translator (toLatex)
    where

import Data.List (intercalate)
import Data.Scientific (floatingOrInteger)
import Moodle.Types (MoodleVal(..))


group :: String -> String
group s = '{':s ++ "}"

-- TODO: Use printf instead of string concatenation
-- | Convert a 'MoodleVal' value into a Latex string.
toLatex :: MoodleVal -> String
toLatex (Number n)
    | n >= 0    = formatted
    | otherwise = group formatted
        where formatted = either show show $ floatingOrInteger n
toLatex (Variable v) = "\\mathrm" ++ group v  -- TODO: make interpolation possible
toLatex (Op op a b) =
    case op of
      "*" -> toLatex a ++ "\\cdot" ++ toLatex b
      "/" -> "\\frac" ++ group (toLatex a) ++ group (toLatex b)
      _   -> group (toLatex a) ++ op ++ group (toLatex b)
toLatex (Function "pi" []) = "\\pi"
toLatex (Function "sqrt" [n]) = "\\sqrt" ++ group (toLatex n)
toLatex (Function "sqrt" [n, r]) =
    "\\sqrt[" ++ toLatex r ++ "]" ++ group (toLatex n)
toLatex (Function "pow" [a, b]) = group (toLatex a) ++ "^" ++ group (toLatex b)
toLatex (Function name xs) = name ++ "(" ++ args ++ ")"
    where args = intercalate ", " $ map toLatex xs
