module Sound.Analysis.Vamp.DynamicLinker.Posix (
    DLL, open, close, symbol
) where

import Foreign.Ptr
import System.Posix.DynamicLinker

type DLL = DL

open :: String -> IO DLL
open = flip dlopen [RTLD_LOCAL, RTLD_LAZY]

close :: DLL -> IO ()
close = dlclose

symbol :: DLL -> String -> IO (FunPtr p)
symbol = dlsym

-- EOF