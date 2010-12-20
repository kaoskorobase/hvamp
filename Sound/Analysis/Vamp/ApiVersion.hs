module Sound.Analysis.Vamp.ApiVersion (
    ApiVersion(..)
  , apiVersion
) where

import Bindings.Sound.Analysis.Vamp (c'VAMP_API_VERSION)
import Foreign
import Foreign.C

newtype ApiVersion = ApiVersion CUInt deriving (Eq, Ord, Read, Show)

apiVersion :: ApiVersion
{-# NOINLINE apiVersion #-}
apiVersion = ApiVersion (unsafePerformIO c'VAMP_API_VERSION)
