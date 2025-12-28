{-# LANGUAGE CApiFFI #-}

module FSPath
  ( FSPath,
    appendPath,
    concatToTextLines,
    fromText,
    isSuffixOf,
    FSPath.null,
    pack,
    toText,
    unsafePackCString,
    FSPath.withCString,
  )
where

import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Foreign as TF
import Foreign.C.String

newtype FSPath = FSPath T.Text
  deriving (Eq, Ord)

instance Show FSPath where
  show = show . toText

trimTrailingSlashes :: T.Text -> T.Text
trimTrailingSlashes (p' T.:> '/') = trimTrailingSlashes p'
trimTrailingSlashes p = p

fromText :: T.Text -> FSPath
fromText = FSPath . trimTrailingSlashes

-- |Copies a cstring to a fspath.
--
-- NOTE: make sure to force evaluation of the result of this function before any subsequent writes
-- to the underlying cstring happen, or before the cstring is freed, etc.
--
-- NOTE: if the cstring is not UTF-8, then any invalid bytes will be replaced with U+FFFD.
--
-- TODO: what we should do is maintain two sets of paths: one is the OS-dependent representation
-- used to do all the filesystems calls, and the other is a UTF-8 string (stored in a Data.Text
-- object) used to do filtering/matching. That way we can incrementally build each one without
-- having to repeatedly encode/decode the same sections of a string, and we also take care of the
-- issue of assuming that posix paths are valid UTF-8.
unsafePackCString :: CString -> IO FSPath
unsafePackCString cstr = do
  bs <- BSU.unsafePackCString cstr
  pure $ fromText $ TE.decodeUtf8Lenient bs

pack :: String -> FSPath
pack = fromText . T.pack

null :: FSPath -> Bool
null (FSPath t) = T.null t

isSuffixOf :: FSPath -> FSPath -> Bool
isSuffixOf (FSPath t1) (FSPath t2) = T.isSuffixOf t1 t2

-- An empty path1 is treated as an implicit "." in that this function will return path2 unmodified.
appendPath :: FSPath -> FSPath -> FSPath
appendPath (FSPath t1) p2@(FSPath t2) =
  if T.null t1
    then p2
    else FSPath $ T.concat [t1, T.singleton '/', t2]

toText :: FSPath -> T.Text
toText (FSPath t) = t

concatToTextLines :: [FSPath] -> T.Text
concatToTextLines = T.concat . map ((flip T.snoc '\n') . toText)

-- An empty path is treated as ".", which allows us to match fd's behavior of omitting './' from the
-- output paths if no search dir is specified, without having to use a Maybe or Either type for the
-- paths in all the path processing functions. There may be a better way to do this but fuck it we
-- ball.
withCString :: FSPath -> (CString -> IO a) -> IO a
withCString (FSPath t) =
  if T.null t
    then
      TF.withCString $ T.singleton '.'
    else
      TF.withCString t
