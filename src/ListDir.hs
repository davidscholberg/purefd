module ListDir
  ( listDir,
  )
where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import DirStream
import Foreign.C.String
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
  let path = BSC.pack pathStr
  pathIsDir <- BS.useAsCString path isDir
  unless
    pathIsDir
    (fail $ "path is not a directory: " ++ pathStr)
  maybeRegex <- maybe (pure Nothing) compileRegex maybeRegexStr
  BS.useAsCString
    path
    (mapStream_ (processDirEntry maybeRegex path) . makeDirStream)

processDirEntry :: Maybe Regex -> BS.ByteString -> CString -> IO ()
processDirEntry maybeRegex basePath dirEntryCStr = do
  dirEntry <- BS.packCString dirEntryCStr
  let path = basePath <> BS.singleton 47 <> dirEntry
  when
    (maybe True (`matchTest` dirEntry) maybeRegex)
    (BS.putStr $ path <> BS.singleton 10)
  BS.useAsCString
    path
    ( \pathCStr -> do
        pathIsDir <- isDir pathCStr
        when
          pathIsDir
          (mapStream_ (processDirEntry maybeRegex path) $ makeDirStream pathCStr)
    )
