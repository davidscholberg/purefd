module ListDirs
  ( listDirs,
  )
where

import Config
import Control.Monad
import qualified Data.ByteString as BS
import Data.List (sort)
import DirStream
import qualified FSPath as F
import Glob
import Stream
import System.Exit
import System.IO
import Text.Regex.TDFA as Regex

listDirs :: Cfg -> IO ()
listDirs (Cfg cfgOpts maybePathMatch inputDirs) = do
  searchDirs <- filterM inputDirFilterer inputDirs
  when
    (null searchDirs)
    exitFailure
  acc <-
    foldStream
      accOrPrintDirEntries
      (Left ([], 0))
      $ parConcatIterate
        makeDirStream'
        (fmap (toNLTerminatedBS . appendPathSep) . matchPath cfgOpts maybePathMatch)
        512
      $ Stream.concat $ makeDirStream <$> searchDirs
  case acc of
    Left (paths, _) -> printDirEntries $ sort paths
    Right () -> pure ()

inputDirFilterer :: F.FSPath -> IO Bool
inputDirFilterer path = do
  pathIsDir <- F.useAsCString path isDir
  if pathIsDir
    then pure True
    else do
      hPutStrLn stderr $ "error: path is not a directory: " ++ show path
      pure False

matchPath :: CfgOptions -> Maybe (Either Regex Glob) -> (F.FSPath, F.FSPath, Bool) -> Maybe (F.FSPath, Bool)
matchPath cfgOpts maybePathMatch (path, dirEntry, pathIsDir) =
  case cfgFilterExtension cfgOpts of
    Just ext ->
      if F.isSuffixOf ext dirEntry
        then go
        else Nothing
    _ ->
      go
  where
    go =
      case maybePathMatch of
        Just (Left regex) ->
          if regex `Regex.matchTest` F.toByteString dirEntry
            then Just (path, pathIsDir)
            else Nothing
        Just (Right glob) ->
          if glob `Glob.matchTest` F.toByteString dirEntry
            then Just (path, pathIsDir)
            else Nothing
        Nothing ->
          Just (path, pathIsDir)

appendPathSep :: (F.FSPath, Bool) -> F.FSPath
appendPathSep (path, pathIsDir) =
  if pathIsDir
    then F.appendPath path $ F.fromByteString BS.empty
    else path

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
