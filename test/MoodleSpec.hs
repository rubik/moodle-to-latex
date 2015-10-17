module MoodleSpec (spec) where

import Test.Hspec
import Test.Hspec.Attoparsec ((~>), shouldParse, shouldFailOn)
import Data.Text (pack)
import Moodle


parse :: String -> Either String MoodleVal
parse = (~> parseExpr) . pack

spec :: Spec
spec = do
    describe "parseExpr" $ do
        it "parses a single integer" $
            parse "2" `shouldParse` Number 2
        it "parses a single float" $
            parse "-2.29" `shouldParse` Number (-2.29)
        it "parses a variable" $
            parse "{var}" `shouldParse` Variable "var"
        it "parses a variable II" $
            parse "{2var}" `shouldParse` Variable "2var"
        it "parses func with no args" $
            parse "pi()" `shouldParse` Function "pi" []
        it "parses func with 1 arg" $
            parse "log(239)" `shouldParse` Function "log" [Number 239]
        it "parses func with 2 args" $
            parse "sqrt(23, 4)" `shouldParse`
                Function "sqrt" [Number 23, Number 4]
        it "parses single addition" $
            parse "2 + -4" `shouldParse` Op "+" (Number 2) (Number (-4))
        it "parses single multiplication" $
            parse "-2 * 4" `shouldParse` Op "*" (Number (-2)) (Number 4)
        it "parses single subtraction" $
            parse "2 - -4" `shouldParse` Op "-" (Number 2) (Number (-4))
        it "parses single division" $
            parse "0 / 32" `shouldParse` Op "/" (Number 0) (Number 32)
        it "parses add/sub assocl" $
            parse "2 + 4 - 3" `shouldParse`
                Op "-" (Op "+" (Number 2) (Number 4)) (Number 3)
        it "parses mul/div assocl" $
            parse "2 / 4 * 45" `shouldParse`
                Op "*" (Op "/" (Number 2) (Number 4)) (Number 45)
        it "parses exp assocr" $
            parse "2^3^4" `shouldParse`
                Op "^" (Number 2) (Op "^" (Number 3) (Number 4))
        it "parses with no spaces" $
            parse "2+3/{va}" `shouldParse`
                Op "+" (Number 2) (Op "/" (Number 3) (Variable "va"))
        it "parses with lots of spaces" $
            parse "  2  +   3  /  {  va  }  " `shouldParse`
                Op "+" (Number 2) (Op "/" (Number 3) (Variable "va"))
        it "parses with lots of spaces in function calls" $
            parse "2 + log ( 24 , 249 )" `shouldParse`
                Op "+" (Number 2) (Function "log" [Number 24, Number 249])
        it "fails on an empty string" $
            parseExpr `shouldFailOn` pack ""
        it "fails on gibberish" $
            parseExpr `shouldFailOn` pack "q 28nureamijf sdk"
    describe "toLatex" $ do
        it "does not group a positive number" $
            toLatex (Number 4) `shouldBe` "4"
        it "groups a negative number" $
            toLatex (Number (-42.2)) `shouldBe` "{-42.2}"
        it "groups a variable into mathrm" $
            toLatex (Variable "bla") `shouldBe` "\\mathrm{bla}"
        it "does not always group multiplication" $
            toLatex (Op "*" (Number 4) (Number (-5))) `shouldBe` "4\\cdot{-5}"
        it "always groups division" $
            toLatex (Op "/" (Number 4) (Number (-5))) `shouldBe`
                "\\frac{4}{{-5}}"
        it "always groups every other operator" $
            toLatex (Op "^" (Number 4) (Number (-5))) `shouldBe` "{4}^{{-5}}"
        it "catches a call to the pi function" $
            toLatex (Function "pi" []) `shouldBe` "\\pi"
        it "correctly renders unary sqrt" $
            toLatex (Function "sqrt" [Number 293]) `shouldBe` "\\sqrt{293}"
        it "correctly renders binary sqrt" $
            toLatex (Function "sqrt" [Number 293, Number 3]) `shouldBe`
                "\\sqrt[3]{293}"
        it "correctly renders binary pow" $
            toLatex (Function "pow" [Number 293, Number 3]) `shouldBe`
                "{293}^{3}"
        it "correctly renders n-ary functions" $
            toLatex (Function "func" [Number 2, Number 3, Number 4]) `shouldBe`
                "func(2, 3, 4)"
