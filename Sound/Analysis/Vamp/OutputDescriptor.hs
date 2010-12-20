module Sound.Analysis.Vamp.OutputDescriptor (
    OutputDescriptor(..)
  , Bins(..)
  , Extents(..)
  , Quantization(..)
  , SampleType(..)
  , peekOutputDescriptor
) where

import           Bindings.Sound.Analysis.Vamp
import qualified Bindings.Sound.Analysis.Vamp.Version2 as Version2
import           Foreign
import           Foreign.C
import           Sound.Analysis.Vamp.Types

data OutputDescriptor = OutputDescriptor {
    identifier :: String
  , name :: String
  , description :: String
  , unit :: String
  , bins :: Maybe Bins
  , extents :: Maybe Extents
  , quantization :: Maybe Quantization
  , sampleType :: SampleType
  , hasDuration :: Bool
  } deriving (Eq, Read, Show)

peekOutputDescriptor ::
    C'VampOutputDescriptor
 -> Maybe Version2.C'VampOutputDescriptor
 -> IO OutputDescriptor
peekOutputDescriptor x v2 = do
    x1 <- peekCString (c'VampOutputDescriptor'identifier x)
    x2 <- peekCString (c'VampOutputDescriptor'name x)
    x3 <- peekCString (c'VampOutputDescriptor'description x)
    x4 <- peekCString (c'VampOutputDescriptor'unit x)
    x5 <- if toBool (c'VampOutputDescriptor'hasFixedBinCount x)
            then do
                let n = fromIntegral (c'VampOutputDescriptor'binCount x)
                if n > 0 && c'VampOutputDescriptor'binNames x /= nullPtr
                    then peekArray n (c'VampOutputDescriptor'binNames x)
                         >>= mapM peekCString
                         >>= return . Just . Bins n
                    else return Nothing
            else return Nothing
    let x6 = if toBool (c'VampOutputDescriptor'hasKnownExtents x)
                then Just (Extents (realToFrac (c'VampOutputDescriptor'minValue x))
                                   (realToFrac (c'VampOutputDescriptor'maxValue x)))
                else Nothing
        x7 = if toBool (c'VampOutputDescriptor'isQuantized x)
                then Just (Quantization (realToFrac (c'VampOutputDescriptor'quantizeStep x)) [])
                else Nothing
        st = c'VampOutputDescriptor'sampleType x
        x8 = if st == c'vampOneSamplePerStep
                then OneSamplePerStep
                else if st == c'vampFixedSampleRate
                     then FixedSampleRate (realToFrac (c'VampOutputDescriptor'sampleRate x))
                     else if st == c'vampVariableSampleRate
                          then VariableSampleRate (realToFrac (c'VampOutputDescriptor'sampleRate x))
                          else error ("Invalid sample type " ++ show st)
        x9 = maybe False ((/=) 0 . Version2.c'VampOutputDescriptor'hasDuration) v2
    return $ OutputDescriptor x1 x2 x3 x4 x5 x6 x7 x8 x9
