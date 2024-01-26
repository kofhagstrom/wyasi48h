module Lexer
  ( Token (..),
    tokens,
  )
where

import Control.Applicative (Alternative (many, (<|>)))
import Parsec (Parser, ignore, manyOf, noneOf, parse, parseWhile, skip)

type Lexer = Parser String [Token]

data Token = LeftBracket | RightBracket | String String | Number String | Symbol String deriving (Eq, Show)

leftBracket :: Parser String Token
leftBracket = parse "(" >> return LeftBracket

rightBracket :: Parser String Token
rightBracket = parse ")" >> return RightBracket

number :: Parser String Token
number = Number <$> manyOf "0123456789"

symbol :: Parser String Token
symbol = Symbol <$> noneOf "() 0123456789"

string :: Parser String Token
string = do
  skip "\""
  s <- noneOf "\""
  skip "\""
  return $ String s

token :: Parser String Token
token =
  ( do
      ignore newline
      token
  )
    <|> ( do
            ignore whitespace
            o <- leftBracket <|> string <|> symbol <|> number <|> rightBracket
            ignore whitespace
            return o
        )
  where
    whitespace = parseWhile (== ' ')
    newline = parse "\n"

tokens :: Lexer
tokens = many token