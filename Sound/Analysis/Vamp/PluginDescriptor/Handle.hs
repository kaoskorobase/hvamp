module Sound.Analysis.Vamp.PluginDescriptor.Handle where

import Bindings.Sound.Analysis.Vamp
import Foreign.Ptr (Ptr)
import Sound.Analysis.Vamp.DynamicLinker (DLL)

data Handle = Handle {
    h_dll :: DLL
  , h_ptr :: Ptr C'VampPluginDescriptor
  } deriving (Show)
