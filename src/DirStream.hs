{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}

module DirStream
  ( CDir,
    closeDir,
    openDir,
    isDir,
    makeDirStream,
    makeDirStream',
    readDirent,
    withDirStream,
  )
where

import Control.Exception
import Control.Monad
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified FSPath as F
import Stream

data {-# CTYPE "DIR" #-} CDir

data DirEntryType = DirEntryDir | DirEntryNotDir | DirEntryUnknown

foreign import capi unsafe "dirent.h opendir" cOpendir :: CString -> IO (Ptr CDir)

foreign import capi unsafe "dirent.h closedir" cClosedir :: Ptr CDir -> IO CInt

foreign import capi unsafe "posix_helper.h purefd_get_dir_entry" cPurefdGetDirEntry :: Ptr CDir -> Ptr CInt -> IO CString

foreign import capi unsafe "posix_helper.h purefd_is_dir" cPurefdIsDir :: CString -> IO CInt

openDir :: CString -> IO (Ptr CDir)
openDir pathCStr = do
  dirPtr <- cOpendir pathCStr
  if dirPtr == nullPtr
    then do
      pathStr <- peekCString pathCStr
      fail $ "couldn't open dir: " ++ pathStr
    else
      pure dirPtr

readDirent :: Ptr CDir -> IO (Maybe (CString, DirEntryType))
readDirent dirPtr =
  alloca
    ( \entryIsDirPtr -> do
        entry <- cPurefdGetDirEntry dirPtr entryIsDirPtr
        errno <- getErrno
        if entry == nullPtr
          then
            if errno /= eOK
              then
                fail "readdir failed"
              else
                pure Nothing
          else
            if entryIsDirPtr == nullPtr
              then
                fail "entryIsDirPtr is null"
              else do
                entryIsDir <- peek entryIsDirPtr
                case entryIsDir of
                  1 -> pure $ Just (entry, DirEntryDir)
                  0 -> pure $ Just (entry, DirEntryNotDir)
                  -1 -> pure $ Just (entry, DirEntryUnknown)
                  _ -> fail $ "unexpected value for entryIsDir :" ++ show entryIsDir
    )

closeDir :: Ptr CDir -> IO ()
closeDir dirPtr = do
  retVal <- cClosedir dirPtr
  when
    (retVal /= 0)
    (fail "couldn't close dir")

isDir :: CString -> IO Bool
isDir path = do
  retVal <- cPurefdIsDir path
  when
    (retVal == -1)
    (fail "dir check failed")
  pure $ retVal /= 0

makeDirStream :: F.FSPath -> Stream (F.FSPath, F.FSPath, Bool)
makeDirStream path =
  Stream
    { open = F.useAsCString path openDir,
      next = \dirPtr -> do
        maybeDirent <- readDirent dirPtr `onException` closeDir dirPtr
        case maybeDirent of
          Just (direntCStr, entryType) -> do
            -- We force dirent to WHNF in the event that this value gets passed to a different
            -- thread where the pointer to direntCStr would no longer be valid.
            !dirent <- F.packCString direntCStr `onException` closeDir dirPtr
            let !path' = F.appendPath path dirent
            case entryType of
              DirEntryDir -> pure $ Just ((path', dirent, True), dirPtr)
              DirEntryNotDir -> pure $ Just ((path', dirent, False), dirPtr)
              DirEntryUnknown -> do
                pathIsDir <- F.useAsCString path' isDir `onException` closeDir dirPtr
                pure $ Just ((path', dirent, pathIsDir), dirPtr)
          Nothing -> pure Nothing,
      close = closeDir
    }

makeDirStream' :: (F.FSPath, F.FSPath, Bool) -> Maybe (Stream (F.FSPath, F.FSPath, Bool))
makeDirStream' (path, _, pathIsDir) =
  if pathIsDir
    then
      Just $ makeDirStream path
    else
      Nothing

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
