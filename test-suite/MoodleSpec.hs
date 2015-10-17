module MoodleSpec (spec) where

import Test.HUnit (Assertion)
import Test.Hspec
import Data.Text (pack)
import Data.Attoparsec.Text (parseOnly)
import Moodle


shouldBeR :: Either String MoodleVal -> MoodleVal -> Assertion
shouldBeR actual expected = actual `shouldBe` Right expected

parse :: String -> Either String MoodleVal
parse = parseOnly parseExpr . pack

spec :: Spec
spec = do
    describe "parseExpr" $ do
        it "parses a single integer" $
            parse "2" `shouldBeR` Number 2
        it "parses a single float" $
            parse "-2.29" `shouldBeR` Number (-2.29)
        it "parses a variable" $
            parse "{var}" `shouldBeR` Variable "var"
        it "parses a variable II" $
            parse "{2var}" `shouldBeR` Variable "2var"
        it "parses func with no args" $
            parse "pi()" `shouldBeR` Function "pi" []
        it "parses func with 1 arg" $
            parse "log(239)" `shouldBeR` Function "log" [Number 239]
        it "parses func with 2 args" $
            parse "sqrt(23, 4)" `shouldBeR`
                Function "sqrt" [Number 23, Number 4]
        it "parses single addition" $
            parse "2 + -4" `shouldBeR` Op "+" (Number 2) (Number (-4))
        it "parses single multiplication" $
            parse "-2 * 4" `shouldBeR` Op "*" (Number (-2)) (Number 4)
        it "parses single subtraction" $
            parse "2 - -4" `shouldBeR` Op "-" (Number 2) (Number (-4))
        it "parses single division" $
            parse "0 / 32" `shouldBeR` Op "/" (Number 0) (Number 32)
        it "parses add/sub assocl" $
            parse "2 + 4 - 3" `shouldBeR`
                Op "-" (Op "+" (Number 2) (Number 4)) (Number 3)
        it "parses mul/div assocl" $
            parse "2 / 4 * 45" `shouldBeR`
                Op "*" (Op "/" (Number 2) (Number 4)) (Number 45)
        it "parses exp assocr" $
            parse "2^3^4" `shouldBeR`
                Op "^" (Number 2) (Op "^" (Number 3) (Number 4))
