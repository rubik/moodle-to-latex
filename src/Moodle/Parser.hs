{-# LANGUAGE OverloadedStrings #-}
module Moodle.Parser (parseExpr)
    where

import Data.Char (isAlphaNum)
import Data.Text (Text, pack)
import Control.Applicative (Alternative, (<|>))
import Data.Attoparsec.Text
import Moodle.Types (MoodleVal(..))


parens :: Parser a -> Parser a
parens p = operator "(" *> p <* operator ")"

operator :: String -> Parser Text
operator op = skipSpace *> string (pack op) <* skipSpace

parseNumber :: Parser MoodleVal
parseNumber = Number <$> (skipSpace *> scientific)

parseVariable :: Parser MoodleVal
parseVariable = Variable <$> var
    where var = operator "{" *> many1' (satisfy isAlphaNum) <* operator "}"

parseFunction :: Parser MoodleVal
parseFunction = Function <$> many1' letter <*>
    parens (parseExpr `sepBy` operator ",")

-- | Parse a Moodle expression. This parser expects 'Text', not a 'String'.
parseExpr :: Parser MoodleVal
parseExpr = mulTerm `chainl1` addAction

mulTerm :: Parser MoodleVal
mulTerm = expTerm `chainl1` multiplyAction

expTerm :: Parser MoodleVal
expTerm = factor `chainr1` opAction "^"

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
chainl1 p op = flip id <$> p <*> rst
    where rst = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id

chainr1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainr1 p op = scanAhead
    where scanAhead = flip id <$> p <*> rst
          rst = (flip <$> op <*> scanAhead) <|> pure id
