{-# OPTIONS_GHC -cpp #-}

module Sound.Analysis.Vamp.DynamicLinker (
    DLL, open, symbol
) where

import Foreign.Ptr
#if defined(mingw32_HOST_OS)
import qualified Sound.Analysis.Vamp.DynamicLinker.Win32 as DL
#else
import qualified Sound.Analysis.Vamp.DynamicLinker.Posix as DL
#endif
import System.Mem.Weak

type DLL = DL.DLL

open :: FilePath -> IO DLL
open path = do
    dll <- DL.open path
    addFinalizer dll (DL.close dll)
    return dll

symbol :: DLL -> String -> IO (FunPtr a)
symbol = DL.symbol
