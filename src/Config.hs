module Config
  ( Cfg (..),
    CfgOptions (..),
    parseCliArgs,
  )
where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import qualified FSPath as F
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString

data CfgOptions
  = CfgMatchAll
  | CfgMatchExt F.FSPath

data Cfg
  = Cfg
      (Maybe Regex)
      F.FSPath
      CfgOptions

data ExpectedOptVal
  = OptNone
  | OptExtention
  deriving Show

data ParseOptState =
  ParseOptState
    CfgOptions
    ExpectedOptVal

type ParseState =
  Either
    ParseOptState
    Cfg

newtype CliArgError = CliArgError String
  deriving Show

instance Exception CliArgError where
  displayException (CliArgError s) = "cli arg error: " ++ s

defaultSearchDir :: F.FSPath
defaultSearchDir = F.pack ""

defaultRegex :: Maybe Regex
defaultRegex = Nothing

defaultCfgOptions :: CfgOptions
defaultCfgOptions = CfgMatchAll

initialParseState :: ParseState
initialParseState = Left $ ParseOptState defaultCfgOptions OptNone

compileRegexStr :: String -> Either CliArgError Regex
compileRegexStr = either (Left . CliArgError) Right . compile defaultCompOpt defaultExecOpt . BSC.pack

parseCliArgs :: [String] -> Either CliArgError Cfg
parseCliArgs args = do
  ps <- foldM parseCliArg initialParseState args
  case ps of
    Left (ParseOptState cfgOpts OptNone) -> Right $ Cfg defaultRegex defaultSearchDir cfgOpts
    Left (ParseOptState _ expectedOpt) -> Left $ CliArgError $ "expected value for opt " ++ show expectedOpt
    Right cfg -> Right cfg

parseCliArg :: ParseState -> String -> Either CliArgError ParseState
parseCliArg _ "" = Left $ CliArgError "empty arg value not allowed"
parseCliArg ps arg =
  case ps of
    Left (ParseOptState _ OptExtention) -> Right $ Left $ ParseOptState (CfgMatchExt $ F.pack $ "." ++ arg) OptNone
    Left (ParseOptState cfgOpts OptNone)
      | arg == "-e" || arg == "--extension" -> Right $ Left $ ParseOptState cfgOpts OptExtention
      | arg == "--" -> Right $ Right $ Cfg defaultRegex defaultSearchDir cfgOpts
      | otherwise -> do
          regex <- compileRegexStr arg
          Right $ Right $ Cfg (Just regex) defaultSearchDir cfgOpts
    Right (Cfg Nothing searchDir cfgOpts) -> do
      regex <- compileRegexStr arg
      Right $ Right $ Cfg (Just regex) searchDir cfgOpts
    Right (Cfg regex searchDir cfgOpts) ->
      if F.null searchDir
        then
          Right $ Right $ Cfg regex (F.pack arg) cfgOpts
        else
          Left $ CliArgError $ "unexpected arg after search dir: \"" ++ arg ++ "\""
