module ListDir
  ( listDir,
  )
where

import Config
import qualified Data.ByteString as BS
import Data.List (sort)
import Data.Maybe
import DirStream
import qualified FSPath as F
import Stream
import Text.Regex.TDFA

listDir :: Cfg -> IO ()
listDir (Cfg maybeRegex path cfgOpts) = do
  pathIsDir <- F.useAsCString path isDir
  if pathIsDir
    then do
      acc <-
        foldStream
          accOrPrintDirEntries
          (Left ([], 0))
          $ parConcatIterate
            makeDirStream'
            (fmap (toNLTerminatedBS . appendPathSep) . matchPath cfgOpts maybeRegex)
            (if isJust maybeRegex then id else (`div` 2))
            512
          $ makeDirStream path
      case acc of
        Left (paths, _) -> printDirEntries $ sort paths
        Right () -> pure ()
    else
      fail $ "path is not a directory: " ++ show path

matchPath :: CfgOptions -> Maybe Regex -> (F.FSPath, F.FSPath, Bool) -> Maybe (F.FSPath, Bool)
matchPath cfgOpts maybeRegex (path, dirEntry, pathIsDir) =
  case cfgOpts of
    CfgMatchExt ext ->
      if F.isSuffixOf ext dirEntry
        then
          go
        else
          Nothing
    _ ->
      go
  where
    go =
      case maybeRegex of
        Just regex ->
          if regex `matchTest` F.toByteString dirEntry
            then
              Just (path, pathIsDir)
            else
              Nothing
        Nothing ->
          Just (path, pathIsDir)

appendPathSep :: (F.FSPath, Bool) -> F.FSPath
appendPathSep (path, pathIsDir) =
  if pathIsDir
    then
      F.appendPath path $ F.fromByteString BS.empty
    else
      path

toNLTerminatedBS :: F.FSPath -> BS.ByteString
toNLTerminatedBS = flip BS.snoc 10 . F.toByteString

accOrPrintDirEntries :: Either ([BS.ByteString], Integer) () -> ([BS.ByteString], Integer) -> IO (Either ([BS.ByteString], Integer) ())
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

printDirEntries :: [BS.ByteString] -> IO ()
printDirEntries = BS.putStr . BS.concat
