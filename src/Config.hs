{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Config
  ( Cfg (..),
    CfgFilterExtension (..),
    CfgOptions (..),
    parseCliArgs,
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Bifunctor
import qualified Data.ByteString.Char8 as BSC
import qualified FSPath as F
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString

newtype CfgFilterExtension = CfgFilterExtension F.FSPath
  deriving (Show)

newtype CfgOptions
  = CfgOptions
  { cfgFilterExtension :: Maybe CfgFilterExtension
  }
  deriving (Show)

data Cfg
  = Cfg
      CfgOptions
      (Maybe Regex)
      F.FSPath

instance Show Cfg where
  show (Cfg opts maybeRegex path) =
    "Cfg ("
      ++ show opts
      ++ ") "
      ++ maybe "Nothing" (const "(Just regex)") maybeRegex
      ++ " ("
      ++ show path
      ++ ")"

newtype CliArgError = CliArgError String
  deriving (Show)

instance Exception CliArgError

data CliArgNoParse = CliArgNoParse
  deriving (Show)

type CliArgParseResult a = Either CliArgError (Either CliArgNoParse (a, [String]))

newtype CliArgParser a = CliArgParser {runCliArgParser :: [String] -> CliArgParseResult a}

instance Functor CliArgParser where
  fmap f (CliArgParser p) = CliArgParser $ fmap (fmap (first f)) . p

instance Applicative CliArgParser where
  pure a = CliArgParser $ Right . Right . (a,)
  CliArgParser pf <*> CliArgParser pa = CliArgParser $ \ss ->
    case pf ss of
      Left e -> Left e
      Right (Left e) -> Right $ Left e
      Right (Right (f, ss')) -> case pa ss' of
        Left e' -> Left e'
        Right (Left e') -> Right $ Left e'
        Right (Right (a, ss'')) -> Right $ Right (f a, ss'')

instance Alternative CliArgParser where
  empty = CliArgParser $ const $ Right $ Left CliArgNoParse
  CliArgParser p1 <|> CliArgParser p2 = CliArgParser $ \ss ->
    case p1 ss of
      Left e -> Left e
      Right (Left _) -> p2 ss
      Right (Right v) -> Right $ Right v

instance Monad CliArgParser where
  CliArgParser p >>= f = CliArgParser $ \ss ->
    case p ss of
      Left e -> Left e
      Right (Left e) -> Right $ Left e
      Right (Right (a, ss')) -> runCliArgParser (f a) ss'

manyCompose :: (Alternative f) => f (a -> a) -> f (a -> a)
manyCompose p = go
  where
    go = liftA2 (.) p go <|> pure id

fatalParse :: String -> CliArgParseResult a
fatalParse = Left . CliArgError

noParse :: CliArgParseResult a
noParse = Right $ Left CliArgNoParse

yesParse :: (a, [String]) -> CliArgParseResult a
yesParse = Right . Right

parseOpt :: String -> CliArgParser String
parseOpt optStr = CliArgParser $ \case
  optStr' : ss ->
    if optStr' == optStr
      then yesParse (optStr, ss)
      else noParse
  _ -> noParse

parseOptWithArg ::
  String ->
  String ->
  (String -> CfgOptions -> CfgOptions) ->
  CliArgParser (CfgOptions -> CfgOptions)
parseOptWithArg shortOpt longOpt optArgF = CliArgParser $ \case
  optStr : ss
    | optStr == shortOpt || optStr == longOpt ->
        case ss of
          argStr : ss' -> yesParse (optArgF argStr, ss')
          _ -> fatalParse $ "expected arg for " ++ optStr ++ " option"
    | otherwise -> noParse
  _ -> noParse

parseFilterExtension :: CliArgParser (CfgOptions -> CfgOptions)
parseFilterExtension =
  parseOptWithArg
    "-e"
    "--extension"
    ( \argStr opts ->
        opts {cfgFilterExtension = Just $ CfgFilterExtension $ F.pack $ "." ++ argStr}
    )

parseEndofOpts :: CliArgParser ()
parseEndofOpts = void $ parseOpt "--"

defaultCfgOptions :: CfgOptions
defaultCfgOptions =
  CfgOptions {cfgFilterExtension = Nothing}

parseCfgOptions :: CliArgParser (CfgOptions -> CfgOptions)
parseCfgOptions =
  manyCompose parseFilterExtension
    <* (parseEndofOpts <|> pure ())

parseNextArg :: CliArgParser String
parseNextArg = CliArgParser $ \case
  argStr : ss -> yesParse (argStr, ss)
  _ -> noParse

parseRegexArg :: CliArgParser Regex
parseRegexArg = CliArgParser $ \case
  argStr : ss ->
    case compile defaultCompOpt defaultExecOpt $ BSC.pack argStr of
      Left e -> fatalParse e
      Right r -> yesParse (r, ss)
  _ -> noParse

parseCfg :: CliArgParser Cfg
parseCfg =
  Cfg
    <$> (parseCfgOptions <*> pure defaultCfgOptions)
    <*> optional parseRegexArg
    <*> (F.pack <$> (parseNextArg <|> pure ""))

parseCliArgs :: [String] -> Either CliArgError Cfg
parseCliArgs args =
  case runCliArgParser parseCfg args of
    Left e -> Left e
    Right (Left _) -> Left $ CliArgError "internal error: no parse"
    Right (Right (cfg, [])) -> Right cfg
    Right (Right (_, ss)) -> Left $ CliArgError $ "unexpected trailing arg(s): " ++ show ss
