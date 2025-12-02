module ListDir
  ( listDir,
  )
where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
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
    then
      mapStream_ (processDirEntry maybeRegex)
        $ concatIterateIO
          ( \(p, _, d) ->
              if d
                then
                  Just $ makeDirStream p
                else
                  Nothing
          )
        $ makeDirStream path
    else
      fail $ "path is not a directory: " ++ pathStr

processDirEntry :: Maybe Regex -> (F.FSPath, F.FSPath, Bool) -> IO ()
processDirEntry maybeRegex (path, dirEntry, _) =
  when
    (maybe True (`matchTest` F.toByteString dirEntry) maybeRegex)
    (BS.putStr $ flip BS.snoc 10 $ F.toByteString path)
