module Test.Moodle
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
    [ testCase "Truth tests" $ Number 2 @==? parse "2"
    ]
