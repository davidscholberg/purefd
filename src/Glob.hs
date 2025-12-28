{-# LANGUAGE LambdaCase #-}

module Glob
  ( compile,
    Glob,
    GlobError (..),
    matchTest,
  )
where

import Control.Applicative
import Control.Exception
import qualified Data.Text as T
import Parser

newtype GlobError = GlobError String
  deriving (Show)

instance Exception GlobError

type GlobParser a = Parser Char a

-- TODO: implementing char classes as a text array of all possible char matches is probably
-- really inefficient if there's a lot of char ranges in a particular class.
data Globeme
  = ExactString T.Text
  | AnyChar
  | AnyString
  | CharClass T.Text
  | NotCharClass T.Text
  deriving (Show)

newtype Glob = Glob [Globeme]
  deriving (Show)

isSpecialChar :: Char -> Bool
isSpecialChar c =
  c == '?'
    || c == '*'
    || c == '['

parseExactString :: GlobParser Globeme
parseExactString = Parser $ \str ->
  go str False T.empty
  where
    go s escaped t =
      case s of
        c : s'
          | isSpecialChar c ->
              if escaped
                then go s' False (T.snoc t c)
                else
                  if T.null t
                    then ParseNo
                    else ParseYes (ExactString t, s)
          | c == '\\' ->
              if escaped
                then go s' True (T.snoc t '\\')
                else go s' True t
          | otherwise ->
              if escaped
                then go s' False (T.snoc (T.snoc t '\\') c)
                else go s' False (T.snoc t c)
        _ ->
          if escaped
            then ParseYes (ExactString $ T.snoc t '\\', s)
            else
              if T.null t
                then ParseNo
                else ParseYes (ExactString t, s)

parseAnyChar :: GlobParser Globeme
parseAnyChar = AnyChar <$ parseMatching '?'

parseAnyString :: GlobParser Globeme
parseAnyString = AnyString <$ parseMatching '*'

appendCharRange :: Char -> Char -> T.Text -> T.Text
appendCharRange c1 c2 =
  go c1
  where
    go c t =
      if c > c2
        then t
        else go (succ c) (T.snoc t c)

parseCharClass :: GlobParser Globeme
parseCharClass = Parser $ \case
  '[' : '!' : ']' : str -> go str NotCharClass $ T.singleton ']'
  '[' : '!' : str -> go str NotCharClass T.empty
  '[' : ']' : str -> go str CharClass $ T.singleton ']'
  '[' : str -> go str CharClass T.empty
  _ -> ParseNo
  where
    go s f t =
      case s of
        c1 : '-' : c2 : s' ->
          if c1 <= c2
            then go s' f $ appendCharRange c1 c2 t
            else ParseError $ "invalid range '" ++ show c1 ++ "' to '" ++ show c2 ++ "'"
        ']' : s' -> ParseYes (f t, s')
        c : s' -> go s' f (T.snoc t c)
        _ -> ParseError "char class not closed"

parseGlob :: GlobParser Glob
parseGlob =
  Glob
    <$> some
      ( parseExactString
          <|> parseAnyString
          <|> parseAnyChar
          <|> parseCharClass
      )

compile :: String -> Either GlobError Glob
compile globStr =
  case runParser parseGlob globStr of
    ParseError e -> Left $ GlobError e
    ParseNo -> Left $ GlobError "invalid glob"
    ParseYes (g, []) -> Right g
    ParseYes (_, _) -> Left $ GlobError "invalid glob"

matchAnyString :: Glob -> T.Text -> Bool
matchAnyString glob =
  go
  where
    go t =
      matchTest glob t
        || ( not (T.null t)
               && (go $! T.drop 1 t)
           )

matchTest :: Glob -> T.Text -> Bool
matchTest (Glob globemes) =
  go globemes
  where
    go [] t = T.null t
    go (AnyChar : gs) t =
      case T.uncons t of
        Just (_, t') -> go gs t'
        Nothing -> False
    go (AnyString : gs) t = matchAnyString (Glob gs) t
    go (ExactString es : gs) t =
      T.isPrefixOf es t
        && (go gs $! T.drop (T.length es) t)
    go (g : gs) t =
      case T.uncons t of
        Just (c, t') ->
          charClassPred c
            && go gs t'
        Nothing -> False
      where
        charClassPred c =
          case g of
            CharClass cs -> T.elem c cs
            NotCharClass cs -> not $ T.elem c cs
