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
    ( \pathCStr ->
        withDirStream pathCStr (processDirent maybeRegex path)
    )

processDirent :: Maybe Regex -> BS.ByteString -> Ptr CDir -> IO ()
processDirent maybeRegex basePath dirStream = do
  maybeDirEntry <- readDirent dirStream
  case maybeDirEntry of
    Nothing -> pure ()
    Just dirEntryCStr -> do
      dirEntry <- BS.packCString dirEntryCStr
      let path = basePath <> BS.singleton 47 <> dirEntry
      when
        (maybe True (`matchTest` dirEntry) maybeRegex)
        (BS.putStr $ path <> BS.singleton 10)
      BS.useAsCString
        path
        ( \pathCStr ->
            withDirStream pathCStr (processDirent maybeRegex path)
        )
      processDirent maybeRegex basePath dirStream
