module ListDir
    ( someFunc
    ) where

import System.Directory.OsPath.Streaming
import System.OsPath

listDir :: String -> IO ()
listDir dirStr = do
  p <- encodeUtf dirStr
  ds <- openDirStream p
  go ds
    where
      go ds' = do
        ioEntry <- readDirStream ds'
        case ioEntry of
          Nothing -> return ()
          Just entry -> do
            putStrLn $ show entry
            go ds'

someFunc :: IO ()
someFunc = listDir "."
