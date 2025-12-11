{-# LANGUAGE CApiFFI #-}

module FSPath
  ( FSPath(..),
    appendPath,
    concatToByteStringLines,
    fromByteString,
    FSPath.null,
    pack,
    packCString,
    toByteString,
    useAsCString,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BSU
import Foreign.C.String
import Foreign.C.Types

foreign import capi unsafe "string.h strlen" cStrlen :: CString -> CSize

newtype FSPath = FSPath BS.ByteString
  deriving (Eq, Ord)

fromByteString :: BS.ByteString -> FSPath
fromByteString = FSPath . addTerminatingNull . trimTrailingSlashes

-- Copies a cstring to a fspath.
-- This function assumes that the cstring does not have a trailing path sep.
packCString :: CString -> IO FSPath
packCString cstr = do
  let strSize = cStrlen cstr
  bs <- BSU.unsafePackCStringLen (cstr, fromIntegral strSize + 1)
  pure $ FSPath $ BS.copy bs

pack :: String -> FSPath
pack = fromByteString . BSC.pack

null :: FSPath -> Bool
null (FSPath bs) = bs == BS.singleton 0

addTerminatingNull :: BS.ByteString -> BS.ByteString
addTerminatingNull = flip BS.snoc 0

addTerminatingNewline :: BS.ByteString -> BS.ByteString
addTerminatingNewline = flip BS.snoc 10

trimTrailingSlashes :: BS.ByteString -> BS.ByteString
trimTrailingSlashes p =
  case BS.unsnoc p of
    Just (p', 47) -> trimTrailingSlashes p'
    _ -> p

-- An empty path1 is treated as an implicit "." in that this function will return path2 unmodified.
appendPath :: FSPath -> FSPath -> FSPath
appendPath (FSPath path1) (FSPath path2) =
  FSPath $
    BS.append
      (case BS.unsnoc path1 of
        Just (path1', 0) ->
          if BS.null path1'
            then
              path1'
            else
              BS.snoc path1' 47
        -- NOTE: This code should be unreachable since FSPaths should always have a terminating null
        -- byte.
        Just (_, _) -> undefined
        Nothing -> undefined)
      path2

toByteString :: FSPath -> BS.ByteString
toByteString (FSPath bs) =
  case BS.unsnoc bs of
    Just (bs', _) -> bs'
    Nothing -> bs

concatToByteStringLines :: [FSPath] -> BS.ByteString
concatToByteStringLines = BS.concat . map (addTerminatingNewline . toByteString)

-- An empty path is treated as ".".
useAsCString :: FSPath -> (CString -> IO a) -> IO a
useAsCString p@(FSPath bs) =
  if FSPath.null p
    then
      BSU.unsafeUseAsCString $ BS.snoc (BS.singleton 46) 0
    else
      BSU.unsafeUseAsCString bs
