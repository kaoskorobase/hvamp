module Sound.Analysis.Vamp.Feature (
    Vector
  , Feature(..)
  , FeatureList
  , TimeStamp(..)
  , peekFeature
  , peekFeatureList
) where

import           Control.Monad
import           Bindings.Sound.Analysis.Vamp
import           Bindings.Sound.Analysis.Vamp.Version2
import qualified Data.Vector.Storable as SV
import           Foreign
import           Foreign.C
import           Sound.Analysis.Vamp.ApiVersion (ApiVersion(..))
import           Sound.Analysis.Vamp.TimeStamp

type Vector = SV.Vector Float

data Feature = Feature {
    time      :: Maybe TimeStamp
  , duration  :: Maybe TimeStamp
  , values    :: Vector
  , label     :: Maybe String
  } deriving (Eq, Show)

type FeatureList = [Feature]

peekVector :: Int -> Ptr CFloat -> IO Vector
peekVector n ptr = do
    fptr <- mallocForeignPtrArray n
    withForeignPtr fptr (\dst -> copyArray dst (castPtr ptr) n)
    return $ SV.unsafeFromForeignPtr fptr 0 n

peekFeature :: C'VampFeature -> Maybe C'VampFeatureV2 -> IO Feature
peekFeature v1 v2 = do
    let x1 = if toBool (c'VampFeature'hasTimestamp v1)
                then Just $ TimeStamp (fromIntegral (c'VampFeature'sec v1))
                                      (fromIntegral (c'VampFeature'nsec v1))
                else Nothing
        x2 = case v2 of
                Nothing -> Nothing
                Just x -> if toBool (c'VampFeatureV2'hasDuration x)
                            then Just $ TimeStamp (fromIntegral (c'VampFeatureV2'durationSec x))
                                                  (fromIntegral (c'VampFeatureV2'durationNsec x))
                            else Nothing
    x3 <- peekVector (fromIntegral (c'VampFeature'valueCount v1))
                     (c'VampFeature'values v1)
    x4 <- maybePeek peekCString (c'VampFeature'label v1)
    return $ Feature x1 x2 x3 x4

peekFeatureList :: ApiVersion -> C'VampFeatureList -> IO FeatureList
peekFeatureList version x = do
    let n = fromIntegral (c'VampFeatureList'featureCount x)
        vs = c'VampFeatureList'features x
    v1 <- liftM (map c'VampFeatureUnion'v1) (peekArray n vs)
    v2 <- if version >= ApiVersion 2
            then liftM (map (Just . c'VampFeatureUnion'v2)) (peekArray n (vs `plusPtr` n))
            else return (replicate n Nothing)
    zipWithM peekFeature v1 v2
