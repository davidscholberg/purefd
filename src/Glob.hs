module Glob
  ( Glob,
    parseGlob,
  )
where

import Control.Exception
import Data.ByteString
import Data.Word
import Parser

newtype GlobError = GlobError String
  deriving (Show)

instance Exception GlobError

-- TODO: if we're gonna want max perf out of this parser (which we likely will if it's the basis for
-- handling .gitignore files), then this parser should probably be a foldM type, which is a little
-- uglier but more performant (in theory).
type GlobParser a = Parser Char a

data Globeme
  = ExactChar Word8
  | ExactString ByteString
  | AnyChar
  | AnyString
  | CharClass ByteString
  | NotCharClass ByteString

newtype Glob = Glob [Globeme]

parseGlob :: String -> Either GlobError Glob
parseGlob globStr = undefined
