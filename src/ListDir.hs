module ListDir
  ( listDir,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.List (sort)
import DirStream
import qualified FSPath as F
import Stream
import Text.Regex.TDFA
import Text.Regex.TDFA.ByteString

compileRegex :: String -> IO (Maybe Regex)
compileRegex regexStr = do
  let eitherRegex = compile defaultCompOpt defaultExecOpt $ BSC.pack regexStr
  case eitherRegex of
    Left e -> fail e
    Right r -> pure $ Just r

listDir :: Maybe String -> String -> IO ()
listDir maybeRegexStr pathStr = do
  maybeRegex <- maybe (pure Nothing) compileRegex maybeRegexStr
  let path = F.pack pathStr
  pathIsDir <- F.useAsCString path isDir
  if pathIsDir
    then do
      acc <-
        foldStream
          accOrPrintDirEntries
          (Left ([], 0))
          $ parConcatIterate
            makeDirStream'
            (matchPath maybeRegex)
            512
          $ makeDirStream path
      case acc of
        Left (paths, _) -> printDirEntries $ sort paths
        Right () -> pure ()
    else
      fail $ "path is not a directory: " ++ pathStr
  where
    matchPath :: Maybe Regex -> (F.FSPath, F.FSPath, Bool) -> Maybe F.FSPath
    matchPath maybeRegex (path, dirEntry, _) =
      case maybeRegex of
        Just regex ->
          if regex `matchTest` F.toByteString dirEntry
            then
              Just path
            else
              Nothing
        Nothing ->
          Just path

accOrPrintDirEntries :: Either ([F.FSPath], Integer) () -> ([F.FSPath], Integer) -> IO (Either ([F.FSPath], Integer) ())
accOrPrintDirEntries outputMode (pathList, pathCount) =
  case outputMode of
    Left (accList, accCount) -> do
      let newAccCount = accCount + pathCount
      if newAccCount > 1024
        then do
          printDirEntries $ pathList ++ accList
          pure $ Right ()
        else
          pure $ Left (pathList ++ accList, newAccCount)
    Right () -> do
      printDirEntries pathList
      pure $ Right ()

printDirEntries :: [F.FSPath] -> IO ()
printDirEntries = BS.putStr . F.concatToByteStringLines
