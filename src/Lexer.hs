module Lexer
  ( Token (..),
    tokens,
  )
where

import Control.Applicative (Alternative (many, (<|>)))
import Parsec (Parser, ignore, manyOf, noneOf, parse, parseWhile, skip)

type Lexer = Parser String Token

data Token = LeftBracket | RightBracket | String String | Number Int | Symbol String deriving (Eq, Show)

leftBracket :: Lexer
leftBracket = do
  skip "("
  return LeftBracket

rightBracket :: Lexer
rightBracket = do
  skip ")"
  return RightBracket

whitespace :: Parser String String
whitespace = parseWhile (== ' ')

number :: Lexer
number = Number . read <$> manyOf "0123456789"

symbol :: Lexer
symbol = Symbol <$> noneOf ") "

string :: Lexer
string = do
  skip "\""
  s <- noneOf "\""
  skip "\""
  return $ String s

newLine :: Parser String String
newLine = parse "\n"

token :: Parser String Token
token =
  ( do
      ignore newLine
      token
  )
    <|> ( do
            ignore whitespace
            o <- leftBracket <|> string <|> symbol <|> number <|> rightBracket
            ignore whitespace
            return o
        )

tokens :: Parser String [Token]
tokens = many token