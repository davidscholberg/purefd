{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Parser
  ( manyCompose,
    Parser (..),
    ParseResult (..),
    parseMatching,
    parseNext,
  )
where

import Control.Applicative

data ParseResult b a
  = ParseError String
  | ParseNo
  | ParseYes (a, [b])

instance Functor (ParseResult s) where
  fmap f r =
    case r of
      ParseError e -> ParseError e
      ParseNo -> ParseNo
      ParseYes (v, xs) -> ParseYes (f v, xs)

-- | Generic parser type that parses a list of type `b` elements to a type `a` value.
newtype Parser b a = Parser {runParser :: [b] -> ParseResult b a}

instance Functor (Parser b) where
  fmap f (Parser p) = Parser $ fmap f . p

instance Applicative (Parser b) where
  pure v = Parser $ ParseYes . (v,)
  Parser pf <*> Parser pv = Parser $ \xs ->
    case pf xs of
      ParseError e -> ParseError e
      ParseNo -> ParseNo
      ParseYes (f, xs') -> case pv xs' of
        ParseError e -> ParseError e
        ParseNo -> ParseNo
        ParseYes (v, xs'') -> ParseYes (f v, xs'')

instance Alternative (Parser b) where
  empty = Parser $ const ParseNo
  Parser p1 <|> Parser p2 = Parser $ \xs ->
    case p1 xs of
      ParseError e -> ParseError e
      ParseNo -> p2 xs
      ParseYes v -> ParseYes v

instance Monad (Parser b) where
  Parser p >>= f = Parser $ \xs ->
    case p xs of
      ParseError e -> ParseError e
      ParseNo -> ParseNo
      ParseYes (v, xs') -> runParser (f v) xs'

-- | Iteratively run the alternative action until it fails, composing the resulting functions together.
-- This is basically a specialized form of Control.Applicative.many that avoids building a list when
-- the results are functions that need to be composed together.
manyCompose :: (Alternative f) => f (a -> a) -> f (a -> a)
manyCompose p = go
  where
    go = liftA2 (.) p go <|> pure id

parseMatching :: (Eq b) => b -> Parser b b
parseMatching x = Parser $ \case
  x' : xs ->
    if x' == x
      then ParseYes (x, xs)
      else ParseNo
  _ -> ParseNo

parseNext :: Parser b b
parseNext = Parser $ \case
  x : xs -> ParseYes(x, xs)
  _ -> ParseNo
