module Sound.Analysis.Vamp.Types (
    Quantization(..)
  , Extents(..)
  , Bins(..)
  , SampleType(..)
  , InputDomain(..)
) where

data Quantization = Quantization {
    quantizeStep :: Float
  , valueNames :: [String]
  } deriving (Eq, Read, Show)

data Extents = Extents {
    minValue :: Float
  , maxValue :: Float
  } deriving (Eq, Read, Show)

data Bins = Bins {
    binCount :: Int
  , binNames :: [String]
  } deriving (Eq, Read, Show)

data SampleType =
    OneSamplePerStep
  | FixedSampleRate { sampleRate :: Float }
  | VariableSampleRate { resolution :: Float }
  deriving (Eq, Read, Show)

data InputDomain =
    TimeDomain
  | FrequencyDomain
  deriving (Eq, Read, Show)
