{-# LANGUAGE CApiFFI #-}

module FSPath
  ( FSPath(..),
    appendPath,
    concatToByteStringLines,
    fromByteString,
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

foreign import capi unsafe "string.h strlen" cStrlen :: CString -> IO CSize

newtype FSPath = FSPath BS.ByteString
  deriving (Eq, Ord)

fromByteString :: BS.ByteString -> FSPath
fromByteString = FSPath . addTerminatingNull . trimTrailingSlashes

-- Copies a cstring to a fspath.
-- This function assumes that the cstring does not have a trailing path sep.
packCString :: CString -> IO FSPath
packCString cstr = do
  strSize <- cStrlen cstr
  bs <- BSU.unsafePackCStringLen (cstr, fromIntegral strSize + 1)
  pure $ FSPath $ BS.copy bs

pack :: String -> FSPath
pack = fromByteString . BSC.pack

addTerminatingNull :: BS.ByteString -> BS.ByteString
addTerminatingNull = flip BS.snoc 0

addTerminatingNewline :: BS.ByteString -> BS.ByteString
addTerminatingNewline = flip BS.snoc 10

trimTrailingSlashes :: BS.ByteString -> BS.ByteString
trimTrailingSlashes p =
  case BS.unsnoc p of
    Just (p', 47) -> trimTrailingSlashes p'
    _ -> p

appendPath :: FSPath -> FSPath -> FSPath
appendPath (FSPath path1) (FSPath path2) =
  FSPath $
    BS.append
      (case BS.unsnoc path1 of
        Just (path1', _) -> BS.snoc path1' 47
        Nothing -> BS.snoc path1 47)
      path2

toByteString :: FSPath -> BS.ByteString
toByteString (FSPath bs) =
  case BS.unsnoc bs of
    Just (bs', _) -> bs'
    Nothing -> bs

concatToByteStringLines :: [FSPath] -> BS.ByteString
concatToByteStringLines = BS.concat . map (addTerminatingNewline . toByteString)

useAsCString :: FSPath -> (CString -> IO a) -> IO a
useAsCString (FSPath bs) = BSU.unsafeUseAsCString bs
