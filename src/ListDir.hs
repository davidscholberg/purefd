module ListDir
  ( listDir,
  )
where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import DirStream
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
  let path = BSC.pack pathStr
  maybeStream <- makeDirStream path
  case maybeStream of
    Just dirStream -> mapStream_ (processDirEntry maybeRegex) $ concatIterateIO (\(p, _) -> makeDirStream p) dirStream
    Nothing -> fail $ "path is not a directory: " ++ pathStr

processDirEntry :: Maybe Regex -> (BS.ByteString, BS.ByteString) -> IO ()
processDirEntry maybeRegex (path, dirEntry) =
  when
    (maybe True (`matchTest` dirEntry) maybeRegex)
    (BS.putStr $ path <> BS.singleton 10)
