{-# LANGUAGE OverloadedStrings #-}
module Moodle.Parser (parseExpr)
    where

import Data.Char (isAlphaNum)
import Data.Text (Text, pack)
import Data.Scientific (Scientific)
import Control.Applicative (Alternative, pure, (<|>), (*>), (<*), (<$>), (<*>))
import Data.Attoparsec.Text
import Moodle.Types (MoodleVal(..))


delim1 :: String -> Parser Text
delim1 d = string (pack d) <* skipSpace

delim2 :: String -> Parser Text
delim2 d = skipSpace *> string (pack d)

parens :: Parser a -> Parser a
parens p = delim1 "(" *> p <* delim2 ")"

operator :: String -> Parser Text
operator op = skipSpace *> string (pack op) <* skipSpace

parseNumber :: Parser MoodleVal
parseNumber = Number <$> scientific

parseVariable :: Parser MoodleVal
parseVariable = Variable <$> var
    where var = delim1 "{" *> many1' (satisfy isAlphaNum) <* delim2 "}"

parseFunction :: Parser MoodleVal
parseFunction = Function <$> many1' letter <*>
    parens (parseExpr `sepBy` operator ",")

parseExpr :: Parser MoodleVal
parseExpr = mulTerm `chainl1` addAction

mulTerm :: Parser MoodleVal
mulTerm = expTerm `chainl1` multiplyAction

expTerm :: Parser MoodleVal
expTerm = factor `chainl1` opAction "^"

factor :: Parser MoodleVal
factor =  parens parseExpr
      <|> parseFunction
      <|> parseVariable
      <|> parseNumber

opAction :: String -> Parser (MoodleVal -> MoodleVal -> MoodleVal)
opAction op = operator op *> pure (Op op)

multiplyAction :: Parser (MoodleVal -> MoodleVal -> MoodleVal)
multiplyAction = opAction "*" <|> opAction "/"

addAction :: Parser (MoodleVal -> MoodleVal -> MoodleVal)
addAction = opAction "+" <|> opAction "-"

chainl1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainl1 p op = scan
    where scan = flip id <$> p <*> rst
          rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id
