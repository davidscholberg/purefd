{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Config
  ( Cfg (..),
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

data CfgOptions
  = CfgOptions
  { cfgFilterExtension :: Maybe F.FSPath,
    cfgGlobMatch :: Bool
  }
  deriving (Show)

data Cfg
  = Cfg
      CfgOptions
      (Maybe (Either Regex F.FSPath))
      [F.FSPath]

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

parseMatchingArg :: String -> CliArgParser String
parseMatchingArg argStr = CliArgParser $ \case
  argStr' : ss ->
    if argStr' == argStr
      then yesParse (argStr, ss)
      else noParse
  _ -> noParse

parseOpt ::
  String ->
  String ->
  (CfgOptions -> CfgOptions) ->
  CliArgParser (CfgOptions -> CfgOptions)
parseOpt shortOpt longOpt parsedF = CliArgParser $ \case
  optStr : ss ->
    if optStr == shortOpt || optStr == longOpt
      then yesParse (parsedF, ss)
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
        opts {cfgFilterExtension = Just $ F.pack $ "." ++ argStr}
    )

parseGlobMatch :: CliArgParser (CfgOptions -> CfgOptions)
parseGlobMatch =
  parseOpt
    "-g"
    "--glob"
    ( \opts ->
        opts {cfgGlobMatch = True}
    )

parseEndofOpts :: CliArgParser ()
parseEndofOpts = void $ parseMatchingArg "--"

defaultCfgOptions :: CfgOptions
defaultCfgOptions =
  CfgOptions
    { cfgFilterExtension = Nothing,
      cfgGlobMatch = False
    }

parseCfgOptions :: CliArgParser CfgOptions
parseCfgOptions =
  ( manyCompose
      ( parseFilterExtension
          <|> parseGlobMatch
      )
      <* (parseEndofOpts <|> pure ())
  )
    <*> pure defaultCfgOptions

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

parsePathMatch :: Bool -> CliArgParser (Either Regex F.FSPath)
parsePathMatch isGlob =
  if isGlob
    then Right . F.pack <$> parseNextArg
    else Left <$> parseRegexArg

parseSearchDirs :: CliArgParser [F.FSPath]
parseSearchDirs = CliArgParser $ \case
  ss@(_ : _) -> yesParse (fmap F.pack ss, [])
  _ -> yesParse ([F.pack ""], [])

parseCfg :: CliArgParser Cfg
parseCfg = do
  cfgOptions <- parseCfgOptions
  Cfg
    cfgOptions
    <$> optional (parsePathMatch (cfgGlobMatch cfgOptions))
    <*> parseSearchDirs

parseCliArgs :: [String] -> Either CliArgError Cfg
parseCliArgs args =
  case runCliArgParser parseCfg args of
    Left e -> Left e
    Right (Left _) -> Left $ CliArgError "internal error: no parse"
    Right (Right (cfg, [])) -> Right cfg
    Right (Right (_, ss)) -> Left $ CliArgError $ "internal error: unexpected trailing arg(s): " ++ show ss
