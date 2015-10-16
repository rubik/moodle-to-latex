module MoodleTests (tests)
    where

import Test.HUnit (Assertion, Test(..), (@=?))
import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as H
import Data.Text (pack)
import Data.Attoparsec.Text (parseOnly)
import Moodle.Parser (parseExpr)
import Moodle.Types (MoodleVal(..))

tests :: IO [C.Test]
tests = return $ map (uncurry H.test) $ concat
    [ basic
    ]

testCase :: String -> Assertion -> (String, Test)
testCase label assertion = (label, TestCase assertion)

parse :: String -> Either String MoodleVal
parse = parseOnly parseExpr . pack

(@==?) :: MoodleVal -> Either String MoodleVal -> Assertion
expected @==? actual = Right expected @=? actual

basic :: [(String, Test)]
basic =
    [ testCase "single number" $ Number 2 @==? parse "2"
    , testCase "single op" $ Op "*" (Number 3) (Number 9) @==? parse "3*9"
    , testCase "func no args" $ Function "pi" [] @==? parse "pi()"
    , testCase "func 1 arg" $ Function "sqrt" [Number 3] @==? parse "sqrt(3)"
    , testCase "func 2 args" $ Function "sqrt" [Number 3, Number 4] @==? parse "sqrt(3, 4)"
    ]
