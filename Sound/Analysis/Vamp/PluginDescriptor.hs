module Sound.Analysis.Vamp.PluginDescriptor (
    PluginDescriptor(..)
  , ApiVersion(..)
  , InputDomain(..)
  , loadLibrary
  , loadDefaultLibraries
) where

import           Control.Monad
import           Bindings.Sound.Analysis.Vamp
import           Foreign
import           Foreign.C
import           Sound.Analysis.Vamp.ApiVersion
import           Sound.Analysis.Vamp.DynamicLinker (DLL)
import qualified Sound.Analysis.Vamp.DynamicLinker as DL
import           Sound.Analysis.Vamp.Path
import           Sound.Analysis.Vamp.ParameterDescriptor (ParameterDescriptor, peekParameterDescriptor)
import           Sound.Analysis.Vamp.PluginDescriptor.Handle (Handle(..))
import           Sound.Analysis.Vamp.Types

data PluginDescriptor = PluginDescriptor {
    libraryPath :: FilePath
  , handle :: Handle
  , vampApiVersion :: ApiVersion
  , identifier :: String
  , name :: String
  , description :: String
  , maker :: String
  , pluginVersion :: Int
  , copyright :: String
  , parameters :: [ParameterDescriptor]
  , programs :: [String]
  , inputDomain :: InputDomain
  } deriving (Show)

peekPluginDescriptor :: FilePath -> DLL -> Ptr C'VampPluginDescriptor -> IO PluginDescriptor
peekPluginDescriptor path dll ptr = do
    x <- peek ptr
    let x1 = ApiVersion (c'VampPluginDescriptor'vampApiVersion x)
    x2 <- peekCString (c'VampPluginDescriptor'identifier x)
    x3 <- peekCString (c'VampPluginDescriptor'name x)
    x4 <- peekCString (c'VampPluginDescriptor'description x)
    x5 <- peekCString (c'VampPluginDescriptor'maker x)
    let x6 = fromIntegral (c'VampPluginDescriptor'pluginVersion x)
    x7 <- peekCString (c'VampPluginDescriptor'copyright x)
    x8 <- peekArray (fromIntegral (c'VampPluginDescriptor'parameterCount x))
                    (c'VampPluginDescriptor'parameters x)
            >>= mapM (\ptr -> peekParameterDescriptor =<< peek ptr)
    x9 <- peekArray (fromIntegral (c'VampPluginDescriptor'programCount x))
                    (c'VampPluginDescriptor'programs x)
            >>= mapM peekCString
    let t = c'VampPluginDescriptor'inputDomain x
        x10 = if t == c'vampTimeDomain
                then TimeDomain
                else if t == c'vampFrequencyDomain
                     then FrequencyDomain
                     else error ("Invalid input domain " ++ show t)
    return $ PluginDescriptor path (Handle dll ptr) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10

data PluginLibrary = PluginLibrary {
    pl_path :: FilePath
  , pl_dll  :: DLL
  , pl_descriptor :: CUInt -> IO (Ptr C'VampPluginDescriptor)
  }

openLibrary :: FilePath -> IO PluginLibrary
openLibrary path = do
    dll <- DL.open path
    fptr <- DL.symbol dll "vampGetPluginDescriptor"
    let f = mK'vampGetPluginDescriptor (castFunPtr fptr)
    v <- c'VAMP_API_VERSION
    return $ PluginLibrary path dll (f v)

getPluginDescriptors :: PluginLibrary -> IO [PluginDescriptor]
getPluginDescriptors lib = loop 0
    where
        loop i = do
            ptr <- pl_descriptor lib i
            if ptr == nullPtr
                then return []
                else do
                    pd <- peekPluginDescriptor (pl_path lib) (pl_dll lib) ptr
                    rest <- loop (i+1)
                    return (pd:rest)

loadLibrary :: FilePath -> IO [PluginDescriptor]
loadLibrary path = openLibrary path >>= getPluginDescriptors

loadDefaultLibraries :: IO [PluginDescriptor]
loadDefaultLibraries = getPluginLibraries >>= liftM concat . mapM loadLibrary
