module Parsec
  ( Parser (..),
    ParseError (..),
    parseWhile,
    parse,
    skip,
    oneOf,
    noneOf,
    ignore,
    parseC,
    orElse,
    manyOf,
  )
where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Functor (($>))
import Prelude hiding (all, pred)

data ParseError = UnexpectedError deriving (Eq, Show)

newtype Parser i o = Parser {run :: i -> Either ([ParseError], i) (o, i)}

instance (Semigroup i) => Functor (Parser i) where
  fmap f p = Parser $ \input -> do
    (x, input') <- run p input
    return (f x, input')

instance (Semigroup i) => Applicative (Parser i) where
  pure a = Parser $ \input -> Right (a, input)
  p1 <*> p2 = Parser $ \input -> do
    (f, input') <- run p1 input
    (a, input'') <- run p2 input'
    return (f a, input'')

instance (Monoid i) => Alternative (Parser i) where
  empty = Parser $ \_ -> Left (mempty, mempty)
  p1 <|> p2 = Parser $ \input ->
    case run p1 input of
      Right a -> Right a
      Left (e, _) -> case run p2 input of
        Right a' -> Right a'
        Left (e', ts') -> Left (e <> e', ts')

instance (Semigroup i) => Monad (Parser i) where
  return = pure
  p >>= f = Parser $ \input -> do
    (a, input') <- run p input
    run (f a) input'

parseC :: (o -> Bool) -> Parser [o] o
parseC predicate = Parser f
  where
    f all@(t : ts) =
      if predicate t
        then Right (t, ts)
        else Left ([UnexpectedError], all)
    f [] = Left ([UnexpectedError], [])

contains :: Eq t => [t] -> t -> Bool
contains [] _ = False
contains (x' : xs) x = (x == x') || xs `contains` x

of_ :: (b -> ([a], b)) -> Parser b [a]
of_ f =
  Parser $ \input -> case f input of
    ([], rest) -> Left ([UnexpectedError], rest)
    (str, rest) -> Right (str, rest)

oneOf :: Eq o => [o] -> Parser [o] o
oneOf options = Parser $ \input -> case input of
  (x : xs) -> if options `contains` x then Right (x, xs) else Left ([UnexpectedError], input)
  [] -> Left ([UnexpectedError], [])

manyOf :: Eq a => [a] -> Parser [a] [a]
manyOf options = of_ (span (options `contains`))

noneOf :: Eq t => [t] -> Parser [t] [t]
noneOf options = of_ (break (options `contains`))

parseWhile :: (a -> Bool) -> Parser [a] [a]
parseWhile f = Parser $ \input ->
  let (str, rest) = span f input
   in Right (str, rest)

parse :: (Eq a) => [a] -> Parser [a] [a]
parse = traverse (parseC . (==))

skip :: Eq a => [a] -> Parser [a] ()
skip pred = parse pred $> ()

ignore :: Functor f => f a -> f ()
ignore p = p $> ()

orElse :: Alternative t => t a -> t a -> t a
orElse = (<|>)