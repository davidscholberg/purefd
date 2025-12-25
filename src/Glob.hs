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
import qualified Data.ByteString.Char8 as BSC
import Parser

newtype GlobError = GlobError String
  deriving (Show)

instance Exception GlobError

type GlobParser a = Parser Char a

-- TODO: implementing char classes as a bytestring of all possible char matches is probably
-- really inefficient if there's a lot of char ranges in a particular class.
data Globeme
  = ExactString BSC.ByteString
  | AnyChar
  | AnyString
  | CharClass BSC.ByteString
  | NotCharClass BSC.ByteString
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
  go str False BSC.empty
  where
    go s escaped bs =
      case s of
        c : s'
          | isSpecialChar c ->
              if escaped
                then go s' False (BSC.snoc bs c)
                else
                  if BSC.null bs
                    then ParseNo
                    else ParseYes (ExactString bs, s)
          | c == '\\' ->
              if escaped
                then go s' True (BSC.snoc bs '\\')
                else go s' True bs
          | otherwise ->
              if escaped
                then go s' False (BSC.snoc (BSC.snoc bs '\\') c)
                else go s' False (BSC.snoc bs c)
        _ ->
          if escaped
            then ParseYes (ExactString $ BSC.snoc bs '\\', s)
            else
              if BSC.null bs
                then ParseNo
                else ParseYes (ExactString bs, s)

parseAnyChar :: GlobParser Globeme
parseAnyChar = AnyChar <$ parseMatching '?'

parseAnyString :: GlobParser Globeme
parseAnyString = AnyString <$ parseMatching '*'

appendCharRange :: Char -> Char -> BSC.ByteString -> BSC.ByteString
appendCharRange c1 c2 =
  go c1
  where
    go c bs =
      if c > c2
        then bs
        else go (succ c) (BSC.snoc bs c)

parseCharClass :: GlobParser Globeme
parseCharClass = Parser $ \case
  '[' : '!' : ']' : str -> go str NotCharClass $ BSC.singleton ']'
  '[' : '!' : str -> go str NotCharClass BSC.empty
  '[' : ']' : str -> go str CharClass $ BSC.singleton ']'
  '[' : str -> go str CharClass BSC.empty
  _ -> ParseNo
  where
    go s f bs =
      case s of
        c1 : '-' : c2 : s' ->
          if c1 <= c2
            then go s' f $ appendCharRange c1 c2 bs
            else ParseError $ "invalid range '" ++ show c1 ++ "' to '" ++ show c2 ++ "'"
        ']' : s' -> ParseYes (f bs, s')
        c : s' -> go s' f (BSC.snoc bs c)
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

matchAnyString :: Glob -> BSC.ByteString -> Bool
matchAnyString glob =
  go
  where
    go bs =
      matchTest glob bs
        || ( not (BSC.null bs)
               && (go $! BSC.drop 1 bs)
           )

matchTest :: Glob -> BSC.ByteString -> Bool
matchTest (Glob globemes) =
  go globemes
  where
    go [] bs = BSC.null bs
    go (AnyChar : gs) bs =
      case BSC.uncons bs of
        Just (_, bs') -> go gs bs'
        Nothing -> False
    go (AnyString : gs) bs = matchAnyString (Glob gs) bs
    go (ExactString es : gs) bs =
      BSC.isPrefixOf es bs
        && (go gs $! BSC.drop (BSC.length es) bs)
    go (g : gs) bs =
      case BSC.uncons bs of
        Just (c, bs') ->
          charClassPred c
            && go gs bs'
        Nothing -> False
      where
        charClassPred c =
          case g of
            CharClass cs -> BSC.elem c cs
            NotCharClass cs -> BSC.notElem c cs
