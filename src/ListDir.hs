module ListDir
  ( someFunc,
  )
where

import Control.Monad
import System.Directory.OsPath
import System.Directory.OsPath.Streaming
import System.OsPath

listDir :: String -> IO ()
listDir pathStr = do
  path <- encodeUtf pathStr
  listDir' path

listDir' :: OsPath -> IO ()
listDir' basePath = do
  dirStream <- openDirStream basePath
  go dirStream
  closeDirStream dirStream
  where
    go dirStream' = do
      maybeDirEntry <- readDirStream dirStream'
      case maybeDirEntry of
        Nothing -> return ()
        Just dirEntry -> do
          let path = basePath </> dirEntry
          pathStr <- decodeUtf path
          putStrLn pathStr
          isDir <- doesDirectoryExist path
          when isDir $ listDir' path
          go dirStream'

someFunc :: IO ()
someFunc = listDir "."
