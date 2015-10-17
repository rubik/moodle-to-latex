module Main (main)
    where

import Data.Text (pack)
import Data.Attoparsec.Text (parseOnly)
import System.Environment (getArgs)
import Moodle


main :: IO ()
main = do
    (line:_) <- getArgs

    case parseOnly parseExpr $ pack line of
        Left er  -> putStrLn $ "Error: " ++ show er
        Right cl -> putStrLn $ toLatex cl
