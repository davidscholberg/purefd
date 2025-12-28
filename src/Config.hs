{-# LANGUAGE LambdaCase #-}

module Config
  ( Cfg (..),
    CfgOptions (..),
    parseCliArgs,
  )
where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.Text as T
import qualified FSPath as F
import Glob
import Parser
import Text.Regex.TDFA as Regex
import Text.Regex.TDFA.Text as RegexT

newtype CliArgError = CliArgError String
  deriving (Show)

instance Exception CliArgError

type CliArgParser a = Parser String a

data CfgOptions
  = CfgOptions
  { cfgFilterExtension :: Maybe F.FSPath,
    cfgGlobMatch :: Bool
  }
  deriving (Show)

data Cfg
  = Cfg
      CfgOptions
      (Maybe (Either Regex Glob))
      [F.FSPath]

parseOpt ::
  String ->
  String ->
  (CfgOptions -> CfgOptions) ->
  CliArgParser (CfgOptions -> CfgOptions)
parseOpt shortOpt longOpt parsedF = Parser $ \case
  optStr : ss ->
    if optStr == shortOpt || optStr == longOpt
      then ParseYes (parsedF, ss)
      else ParseNo
  _ -> ParseNo

parseOptWithArg ::
  String ->
  String ->
  (String -> CfgOptions -> CfgOptions) ->
  CliArgParser (CfgOptions -> CfgOptions)
parseOptWithArg shortOpt longOpt optArgF = Parser $ \case
  optStr : ss
    | optStr == shortOpt || optStr == longOpt ->
        case ss of
          argStr : ss' -> ParseYes (optArgF argStr, ss')
          _ -> ParseError $ "expected arg for " ++ optStr ++ " option"
    | otherwise -> ParseNo
  _ -> ParseNo

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
parseEndofOpts = void $ parseMatching "--"

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

parseRegexArg :: CliArgParser Regex
parseRegexArg = Parser $ \case
  argStr : ss ->
    case RegexT.compile defaultCompOpt defaultExecOpt $ T.pack argStr of
      Left e -> ParseError e
      Right r -> ParseYes (r, ss)
  _ -> ParseNo

parseGlobArg :: CliArgParser Glob
parseGlobArg = Parser $ \case
  argStr : ss ->
    case Glob.compile argStr of
      Left (GlobError e) -> ParseError e
      Right g -> ParseYes (g, ss)
  _ -> ParseNo

parsePathMatch :: Bool -> CliArgParser (Either Regex Glob)
parsePathMatch isGlob =
  if isGlob
    then Right <$> parseGlobArg
    else Left <$> parseRegexArg

parseSearchDirs :: CliArgParser [F.FSPath]
parseSearchDirs = Parser $ \case
  ss@(_ : _) -> ParseYes (fmap F.pack ss, [])
  _ -> ParseYes ([F.pack ""], [])

parseCfg :: CliArgParser Cfg
parseCfg = do
  cfgOptions <- parseCfgOptions
  Cfg
    cfgOptions
    <$> optional (parsePathMatch (cfgGlobMatch cfgOptions))
    <*> parseSearchDirs

parseCliArgs :: [String] -> Either CliArgError Cfg
parseCliArgs args =
  case runParser parseCfg args of
    ParseError e -> Left $ CliArgError e
    ParseNo -> Left $ CliArgError "internal error: no parse"
    ParseYes (cfg, []) -> Right cfg
    ParseYes (_, ss) -> Left $ CliArgError $ "internal error: unexpected trailing arg(s): " ++ show ss
