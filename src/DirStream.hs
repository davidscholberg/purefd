{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TupleSections #-}

module DirStream
  ( CDir,
    closeDir,
    openDir,
    isDir,
    makeDirStream,
    readDirent,
    withDirStream,
  )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Control.Exception
import Control.Monad
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Stream

data {-# CTYPE "DIR" #-} CDir

foreign import capi unsafe "dirent.h opendir" cOpendir :: CString -> IO (Ptr CDir)

foreign import capi unsafe "dirent.h closedir" cClosedir :: Ptr CDir -> IO CInt

foreign import capi unsafe "posix_helper.h ripfd_get_dir_entry" cRipfdGetDirEntry :: Ptr CDir -> IO CString

foreign import capi unsafe "posix_helper.h ripfd_is_dir" cRipfdIsDir :: CString -> IO CInt

openDir :: CString -> IO (Ptr CDir)
openDir pathCStr = do
  dirPtr <- cOpendir pathCStr
  if dirPtr == nullPtr
    then do
      pathStr <- peekCString pathCStr
      fail $ "couldn't open dir: " ++ pathStr
    else
      pure dirPtr

readDirent :: Ptr CDir -> IO (Maybe CString)
readDirent dirPtr = do
  entry <- cRipfdGetDirEntry dirPtr
  errno <- getErrno
  if entry == nullPtr
    then
      if errno /= eOK
        then
          fail "readdir failed"
        else
          pure Nothing
    else
      pure $ Just entry

closeDir :: Ptr CDir -> IO ()
closeDir dirPtr = do
  retVal <- cClosedir dirPtr
  when
    (retVal /= 0)
    (fail "couldn't close dir")

isDir :: CString -> IO Bool
isDir path = do
  retVal <- cRipfdIsDir path
  when
    (retVal == -1)
    (fail "dir check failed")
  pure $ retVal /= 0

makeDirStream :: BS.ByteString -> IO (Maybe (Stream (BS.ByteString, BS.ByteString)))
makeDirStream path = do
  -- TODO: it absolutely sucks balls that we have to copy the path twice for each directory found.
  -- Ideally ByteString would just be backed by a cstring which would obviate the need for all this
  -- copying (unless maybe the garbage collector might shuffle things around during foreign calls).
  -- Here's what we do: use the unsafe ByteString api to pass bytestrings to cstring functions
  -- without any copying, and then have an abstraction over ByteStrings that concats directory paths
  -- and null terminates the ByteStrings (which is required by the unsafe shit).
  pathIsDir <- BS.useAsCString path isDir
  if pathIsDir
    then
      pure $ Just Stream
        { open = BS.useAsCString path openDir,
          next = \dirPtr -> do
            maybeDirent <- readDirent dirPtr `onException` closeDir dirPtr
            case maybeDirent of
              Just direntCStr -> do
                dirent <- BSC.packCString direntCStr `onException` closeDir dirPtr
                let path' = path <> BS.singleton 47 <> dirent
                pure $ Just ((path', dirent),dirPtr)
              Nothing -> pure Nothing,
          close = closeDir
        }
    else
      pure Nothing

withDirStream :: CString -> (Ptr CDir -> IO ()) -> IO ()
withDirStream path f = do
  pathIsDir <- isDir path
  when
    pathIsDir
    ( do
        dirStream <- openDir path
        f dirStream
        closeDir dirStream
    )
