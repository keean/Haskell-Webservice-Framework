{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- Copyright (C) 2002 Keean Schupke

module Lib.Linker.Linker(MonadLinker(..)) where

import Foreign.C
import GHC.Ptr
import GHC.Exts (addrToAny#)
import Control.Monad (when)
import Lib.Monad.MonadT

class Monad m => MonadLinker m where
    linkerInit :: m ()
    linkerLoadObj :: String -> m ()
    linkerResolveObjs :: m ()
    linkerLookupSymbol :: String -> String -> m (Maybe a)
    linkerUnloadObj :: String -> m ()

instance MonadLinker IO where
    linkerInit = _linkerInit
    linkerLoadObj = _linkerLoadObj
    linkerResolveObjs = _linkerResolveObjs
    linkerLookupSymbol = _linkerLookupSymbol
    linkerUnloadObj = _linkerUnloadObj

instance (MonadLinker m,MonadT t m) => MonadLinker (t m) where
    linkerInit = up $ linkerInit
    linkerLoadObj = up . linkerLoadObj
    linkerResolveObjs = up $ linkerResolveObjs
    linkerLookupSymbol objs sym = up $ linkerLookupSymbol objs sym
    linkerUnloadObj = up . linkerUnloadObj

{-# NOINLINE _linkerInit #-}
_linkerInit :: IO ()
_linkerInit = initLinker

_linkerLoadObj :: String -> IO ()
_linkerLoadObj obj = withCString obj $ \c_obj -> do
    status <- loadObj c_obj
    when (status == 0) (fail $ showString obj ": could not load object")

{-# NOINLINE _linkerResolveObjs #-}
_linkerResolveObjs :: IO ()
_linkerResolveObjs = do
    status <- resolveObjs
    when (status == 0) (fail "could not resolve objects")

{-# NOINLINE _linkerLookupSymbol #-}
_linkerLookupSymbol :: String -> String -> IO (Maybe a)
_linkerLookupSymbol obj sym =
    (withCString (showString obj . showChar '_' . showString sym $ "_closure") $ \c_objsym -> do
    hobj <- lookupSymbol c_objsym
    case hobj of
        o | o == nullPtr -> return Nothing 
        Ptr o -> case (addrToAny# o) of
            (# h #) -> return (Just h)) where
    
{-
    showObjName :: [String] -> ShowS
    showObjName (n0:ns@(_:_)) = showString n0 . showString "zi" . showObjName ns
    showObjName (n0:[]) = showString n0
    showObjName [] = id
-}

{-# NOINLINE _linkerUnloadObj #-}
_linkerUnloadObj :: String -> IO ()
_linkerUnloadObj obj = withCString obj $ \c_obj -> do
    status <- unloadObj c_obj
    when (status == 0) (fail $ (showString "could not unload object: " . showString obj) "")

foreign import ccall "initLinker" initLinker :: (IO ()) 

foreign import ccall "loadObj" loadObj :: CString -> IO Int

foreign import ccall "resolveObjs" resolveObjs :: IO Int

foreign import ccall "lookupSymbol" lookupSymbol :: CString -> IO (Ptr a)

foreign import ccall "unloadObj" unloadObj :: CString -> IO Int
