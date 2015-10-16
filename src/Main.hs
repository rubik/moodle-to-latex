module Main (main)
    where

import Data.Text (pack)
import Data.Attoparsec.Text (parseOnly)
import Moodle.Parser (parseExpr)
import Moodle.Translator (toLatex)


main :: IO ()
main = do
  input <- getLine

  case parseOnly parseExpr $ pack input of
    Left er  -> putStrLn $ "Error: " ++ show er
    Right cl -> putStrLn $ toLatex cl

  main
