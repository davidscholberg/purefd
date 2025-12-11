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
            -- TODO: we could potentially just convert to regular newline terminated bytestrings
            -- here instead of in the main thread.
            (appendPathSep . matchPath maybeRegex)
            512
          $ makeDirStream path
      case acc of
        Left (paths, _) -> printDirEntries $ sort paths
        Right () -> pure ()
    else
      fail $ "path is not a directory: " ++ pathStr

matchPath :: Maybe Regex -> (F.FSPath, F.FSPath, Bool) -> Maybe (F.FSPath, Bool)
matchPath maybeRegex (path, dirEntry, pathIsDir) =
  case maybeRegex of
    Just regex ->
      if regex `matchTest` F.toByteString dirEntry
        then
          Just (path, pathIsDir)
        else
          Nothing
    Nothing ->
      Just (path, pathIsDir)

appendPathSep :: Maybe (F.FSPath, Bool) -> Maybe F.FSPath
appendPathSep maybeElement =
  case maybeElement of
    Just (path, pathIsDir) ->
      if pathIsDir
        then
          Just $ F.appendPath path $ F.fromByteString BS.empty
        else
          Just path
    Nothing -> Nothing

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
