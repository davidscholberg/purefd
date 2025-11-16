{-# LANGUAGE CApiFFI #-}

module DirStream
  ( CDir,
    closeDir,
    openDir,
    isDir,
    readDirent,
    withDirStream,
  )
where

import Control.Monad
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

data {-# CTYPE "DIR" #-} CDir

foreign import capi unsafe "dirent.h opendir" cOpendir :: CString -> IO (Ptr CDir)

foreign import capi unsafe "dirent.h closedir" cClosedir :: Ptr CDir -> IO CInt

foreign import capi unsafe "posix_helper.h fh_get_dir_entry" cFhGetDirEntry :: Ptr CDir -> IO CString

foreign import capi unsafe "posix_helper.h fh_is_dir" cFhIsDir :: CString -> IO CInt

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
  entry <- cFhGetDirEntry dirPtr
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
  retVal <- cFhIsDir path
  when
    (retVal == -1)
    (fail "dir check failed")
  pure $ retVal /= 0

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
