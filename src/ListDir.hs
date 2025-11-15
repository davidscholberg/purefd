module ListDir
  ( listDir,
    listDir',
  )
where

import Control.Monad
import System.Directory.OsPath
import System.Directory.OsPath.Streaming
import System.OsPath
import Text.Regex.TDFA

listDir :: Maybe String -> String -> IO ()
listDir maybeRegex pathStr = do
  path <- encodeUtf pathStr
  listDir' maybeRegex path

listDir' :: Maybe String -> OsPath -> IO ()
listDir' maybeRegex basePath = do
  dirStream <- openDirStream basePath
  go dirStream
  closeDirStream dirStream
  where
    go dirStream' = do
      maybeDirEntry <- readDirStream dirStream'
      case maybeDirEntry of
        Nothing -> pure ()
        Just dirEntry -> do
          let path = basePath </> dirEntry
          pathStr <- decodeUtf path
          when
            (maybe True (pathStr =~) maybeRegex)
            (putStrLn pathStr)
          isDir <- doesDirectoryExist path
          when
            isDir
            (listDir' maybeRegex path)
          go dirStream'
