module Sound.Analysis.Vamp.DynamicLinker.Posix (
    DLL, open, close, symbol
) where

-- untested!

import Control.Monad (liftM)
import Foreign.Ptr
import System.Win32.DLL

type DLL = HINSTANCE

open :: String -> IO DLL
open = loadLibrary

close :: DLL -> IO ()
close = freeLibrary

symbol :: DLL -> String -> IO (FunPtr p)
symbol = liftM castPtrToFunPtr . getProcAddress

-- EOF