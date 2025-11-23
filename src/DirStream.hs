{-# LANGUAGE CApiFFI #-}

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

import Control.Exception
import Control.Monad
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import qualified FSPath as F
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

makeDirStream :: F.FSPath -> IO (Maybe (Stream (F.FSPath, F.FSPath)))
makeDirStream path = do
  pathIsDir <- F.useAsCString path isDir
  if pathIsDir
    then
      pure $ Just Stream
        { open = F.useAsCString path openDir,
          next = \dirPtr -> do
            maybeDirent <- readDirent dirPtr `onException` closeDir dirPtr
            case maybeDirent of
              Just direntCStr -> do
                dirent <- F.packCString direntCStr `onException` closeDir dirPtr
                let path' = F.appendPath path dirent
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
