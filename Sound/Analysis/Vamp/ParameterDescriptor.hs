module Sound.Analysis.Vamp.ParameterDescriptor (
    ParameterDescriptor(..)
  , Extents(..)
  , Quantization(..)
  , peekParameterDescriptor
) where

import Bindings.Sound.Analysis.Vamp
import Foreign
import Foreign.C
import Sound.Analysis.Vamp.Types

data ParameterDescriptor = ParameterDescriptor {
    identifier :: String
  , name :: String
  , description :: String
  , unit :: String
  , extents :: Extents
  , defaultValue :: Float
  , quantization :: Maybe Quantization
  } deriving (Eq, Show)

peekParameterDescriptor :: C'VampParameterDescriptor -> IO ParameterDescriptor
peekParameterDescriptor x = do
    x1  <- peekCString (c'VampParameterDescriptor'identifier x)
    x2  <- peekCString (c'VampParameterDescriptor'name x)
    x3  <- peekCString (c'VampParameterDescriptor'description x)
    x4  <- peekCString (c'VampParameterDescriptor'unit x)
    let x5 = Extents (realToFrac (c'VampParameterDescriptor'minValue x))
                     (realToFrac (c'VampParameterDescriptor'maxValue x))
        x6 = realToFrac (c'VampParameterDescriptor'defaultValue x)
    x7 <- if toBool (c'VampParameterDescriptor'isQuantized x)
            then do
                let res = realToFrac (c'VampParameterDescriptor'quantizeStep x)
                    arr = c'VampParameterDescriptor'valueNames x
                names <- if arr == nullPtr
                            then return []
                            else peekArray0 nullPtr arr >>= mapM peekCString
                return (Just (Quantization res names))
            else return Nothing
    return (ParameterDescriptor x1 x2 x3 x4 x5 x6 x7)
