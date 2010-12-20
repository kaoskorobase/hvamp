module Sound.Analysis.Vamp.Plugin (
    instantiate
  , cleanup
  , initialise
  , reset
  , getParameter
  , setParameter
  , getCurrentProgram
  -- , getProgramName
  , selectProgram
  -- , setProgramByName
  , getPreferredStepSize
  , getPreferredBlockSize
  , getMinChannelCount
  , getMaxChannelCount
  , getChannelBounds
  , getOutputCount
  , getOutputDescriptors
  , process
  , getRemainingFeatures
) where

import           Control.Monad (liftM)
import           Data.Complex (Complex)
import qualified Data.Vector.Storable as SV
import           Bindings.Sound.Analysis.Vamp
import           Foreign
import           Foreign.C
import           Foreign.Storable.Complex ()
import           Sound.Analysis.Vamp.ApiVersion (ApiVersion(..))
import           Sound.Analysis.Vamp.Feature
import           Sound.Analysis.Vamp.OutputDescriptor
import           Sound.Analysis.Vamp.PluginDescriptor (PluginDescriptor)
import qualified Sound.Analysis.Vamp.PluginDescriptor as PD
import           Sound.Analysis.Vamp.PluginDescriptor.Handle (Handle(..))
import           Sound.Analysis.Vamp.Types (InputDomain(..))

data Interface = Interface {
    c'descriptor :: Ptr C'VampPluginDescriptor
  , c'instantiate :: Ptr C'VampPluginDescriptor -> CFloat -> IO C'VampPluginHandle
  , c'cleanup :: C'VampPluginHandle -> IO ()
  , c'initialise :: C'VampPluginHandle -> CUInt -> CUInt -> CUInt -> IO CInt
  , c'reset :: C'VampPluginHandle -> IO ()
  , c'getParameter :: C'VampPluginHandle -> CInt -> IO CFloat
  , c'setParameter :: C'VampPluginHandle -> CInt -> CFloat -> IO ()
  , c'getCurrentProgram :: C'VampPluginHandle -> IO CUInt
  , c'selectProgram :: C'VampPluginHandle -> CUInt -> IO ()
  , c'getPreferredStepSize :: C'VampPluginHandle -> IO CUInt
  , c'getPreferredBlockSize :: C'VampPluginHandle -> IO CUInt
  , c'getMinChannelCount :: C'VampPluginHandle -> IO CUInt
  , c'getMaxChannelCount :: C'VampPluginHandle -> IO CUInt
  , c'getOutputCount :: C'VampPluginHandle -> IO CUInt
  , c'getOutputDescriptor :: C'VampPluginHandle -> CUInt -> IO (Ptr C'VampOutputDescriptor)
  , c'releaseOutputDescriptor :: Ptr C'VampOutputDescriptor -> IO ()
  , c'process :: C'VampPluginHandle -> Ptr (Ptr CFloat) -> CInt -> CInt -> IO (Ptr C'VampFeatureList)
  , c'getRemainingFeatures :: C'VampPluginHandle -> IO (Ptr C'VampFeatureList)
  , c'releaseFeatureSet :: Ptr C'VampFeatureList -> IO ()
  }

instance Show Interface where
    show i = "mkInterface " ++ show (c'descriptor i)

data Plugin = Plugin {
    descriptor :: PluginDescriptor
  , interface  :: Interface
  , handle     :: C'VampPluginHandle
  } deriving (Show)

mkInterface :: Ptr C'VampPluginDescriptor -> IO Interface
mkInterface ptr = do
    p <- peek ptr
    return Interface {
        c'descriptor = ptr
      , c'instantiate = lift mK'instantiate c'VampPluginDescriptor'instantiate p
      , c'cleanup = lift mK'cleanup c'VampPluginDescriptor'cleanup p
      , c'initialise = lift mK'initialise c'VampPluginDescriptor'initialise p
      , c'reset = lift mK'reset c'VampPluginDescriptor'reset p
      , c'getParameter = lift mK'getParameter c'VampPluginDescriptor'getParameter p
      , c'setParameter = lift mK'setParameter c'VampPluginDescriptor'setParameter p
      , c'getCurrentProgram = lift mK'getCurrentProgram c'VampPluginDescriptor'getCurrentProgram p
      , c'selectProgram = lift mK'selectProgram c'VampPluginDescriptor'selectProgram p
      , c'getPreferredStepSize = lift mK'getPreferredStepSize c'VampPluginDescriptor'getPreferredStepSize p
      , c'getPreferredBlockSize = lift mK'getPreferredBlockSize c'VampPluginDescriptor'getPreferredBlockSize p
      , c'getMinChannelCount = lift mK'getMinChannelCount c'VampPluginDescriptor'getMinChannelCount p
      , c'getMaxChannelCount = lift mK'getMaxChannelCount c'VampPluginDescriptor'getMaxChannelCount p
      , c'getOutputCount = lift mK'getOutputCount c'VampPluginDescriptor'getOutputCount p
      , c'getOutputDescriptor = lift mK'getOutputDescriptor c'VampPluginDescriptor'getOutputDescriptor p
      , c'releaseOutputDescriptor = lift mK'releaseOutputDescriptor c'VampPluginDescriptor'releaseOutputDescriptor p
      , c'process = lift mK'process c'VampPluginDescriptor'process p
      , c'getRemainingFeatures = lift mK'getRemainingFeatures c'VampPluginDescriptor'getRemainingFeatures p
      , c'releaseFeatureSet = lift mK'releaseFeatureSet c'VampPluginDescriptor'releaseFeatureSet p
      }
    where
        lift = (.)

instantiate :: PluginDescriptor -> Float -> IO Plugin
instantiate pd sr = do
    i <- mkInterface (h_ptr (PD.handle pd))
    h <- c'instantiate i
            (h_ptr (PD.handle pd))
            (realToFrac sr)
    return $ Plugin pd i h

lift_ :: (Interface -> a) -> Plugin -> a
lift_ f = f . interface

lift :: (Interface -> C'VampPluginHandle -> a) -> Plugin -> a
lift f p = lift_ f p (handle p)

cleanup :: Plugin -> IO ()
cleanup = lift c'cleanup

initialise :: Plugin -> Int -> Int -> Int -> IO Bool
initialise p inputChannels stepSize blockSize = do
    liftM (toBool) $ lift c'initialise p
                        (fromIntegral inputChannels)
                        (fromIntegral stepSize)
                        (fromIntegral blockSize)

reset :: Plugin -> IO ()
reset = lift c'reset

getParameter :: Plugin -> Int -> IO Float
getParameter p = liftM realToFrac . lift c'getParameter p . fromIntegral

setParameter :: Plugin -> Int -> Float -> IO ()
setParameter p i = lift c'setParameter p (fromIntegral i) . realToFrac

getCurrentProgram :: Plugin -> IO Int
getCurrentProgram = liftM fromIntegral . lift c'getCurrentProgram

-- getProgramName :: Plugin -> IO String
-- getProgramName p = do
--     i <- (getProgram p)
--     return $ (pluginPrograms $ pluginDescriptor p) !! i

selectProgram :: Plugin -> Int -> IO ()
selectProgram p = lift c'selectProgram p . fromIntegral

-- setProgramByName :: Plugin -> String -> IO ()
-- setProgramByName p s =
--     case elemIndex s (pluginPrograms $ pluginDescriptor p) of
--         Nothing -> error "invalid program name"
--         Just i -> setProgram p i

getPreferredStepSize :: Plugin -> IO Int
getPreferredStepSize = liftM fromIntegral . lift c'getPreferredStepSize

getPreferredBlockSize :: Plugin -> IO Int
getPreferredBlockSize = liftM fromIntegral . lift c'getPreferredBlockSize

getMinChannelCount :: Plugin -> IO Int
getMinChannelCount = liftM fromIntegral . lift c'getMinChannelCount

getMaxChannelCount :: Plugin -> IO Int
getMaxChannelCount = liftM fromIntegral . lift c'getMaxChannelCount

getChannelBounds :: Plugin -> IO (Int, Int)
getChannelBounds p = do
    cmin <- getMinChannelCount p
    cmax <- getMaxChannelCount p
    return (cmin, cmax)

getOutputCount :: Plugin -> IO Int
getOutputCount = liftM fromIntegral . lift c'getOutputCount

getOutputDescriptor :: Plugin -> Int -> IO OutputDescriptor
getOutputDescriptor p i = do
    ptr <- lift c'getOutputDescriptor p (fromIntegral i)
    od1 <- peek ptr
    od2 <- if PD.vampApiVersion (descriptor p) >= ApiVersion 2
            then liftM Just (peek (castPtr ptr))
            else return Nothing
    od <- peekOutputDescriptor od1 od2
    lift_ c'releaseOutputDescriptor p ptr
    return od

getOutputDescriptors :: Plugin -> IO [OutputDescriptor]
getOutputDescriptors p = do
    n <- getOutputCount p
    mapM (getOutputDescriptor p) [0..n-1]

unsafeProcess :: Storable a => Plugin -> [SV.Vector a] -> TimeStamp -> IO FeatureList
unsafeProcess p bs t =
    withMany SV.unsafeWith bs $ \ptrs ->
        withArray ptrs $ \ptrArr -> do
            ptr <- lift c'process p
                    (castPtr ptrArr)
                    (fromIntegral (sec t))
                    (fromIntegral (nsec t))
            features <- peekFeatureList (PD.vampApiVersion (descriptor p)) =<< peek ptr
            lift_ c'releaseFeatureSet p ptr
            return features

data Input =
    TimeDomainInput [SV.Vector Float]
  | FrequencyDomainInput [SV.Vector (Complex Float)]
  deriving (Eq, Show)

process :: Plugin -> Input -> TimeStamp -> IO FeatureList
process p i t =
    case i of
        TimeDomainInput bs ->
            if inputDomain /= TimeDomain
                then error $ "Input domain mismatch: Expected TimeDomain, got " ++ show inputDomain
                else unsafeProcess p bs t
        FrequencyDomainInput bs ->
            if inputDomain /= FrequencyDomain
                then error $ "Input domain mismatch: Expected FrequencyDomain, got " ++ show inputDomain
                else unsafeProcess p bs t
    where
        inputDomain = PD.inputDomain (descriptor p)

getRemainingFeatures :: Plugin -> IO FeatureList
getRemainingFeatures p = do
    ptr <- lift c'getRemainingFeatures p
    features <- peekFeatureList (PD.vampApiVersion (descriptor p)) =<< peek ptr
    lift_ c'releaseFeatureSet p ptr
    return features
