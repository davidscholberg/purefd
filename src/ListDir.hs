module ListDir
  ( listDir,
  )
where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import DirStream
import Foreign.Ptr
import Text.Regex.TDFA

listDir :: Maybe String -> String -> IO ()
listDir maybeRegex pathStr = do
  let path = BSC.pack pathStr
  pathIsDir <- BS.useAsCString path isDir
  unless
    pathIsDir
    (fail $ "path is not a directory: " ++ pathStr)
  BS.useAsCString
    path
    ( \pathCStr ->
        withDirStream pathCStr (processDirent maybeRegex path)
    )

processDirent :: Maybe String -> BS.ByteString -> Ptr CDir -> IO ()
processDirent maybeRegex basePath dirStream = do
  maybeDirEntry <- readDirent dirStream
  case maybeDirEntry of
    Nothing -> pure ()
    Just dirEntryCStr -> do
      dirEntry <- BS.packCString dirEntryCStr
      let path = basePath <> BS.singleton 47 <> dirEntry
      when
        (maybe True (dirEntry =~) maybeRegex)
        (BS.putStr $ path <> BS.singleton 10)
      BS.useAsCString
        path
        ( \pathCStr ->
            withDirStream pathCStr (processDirent maybeRegex path)
        )
      processDirent maybeRegex basePath dirStream
