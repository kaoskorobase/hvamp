module Sound.Analysis.Vamp.TimeStamp (
    TimeStamp(..)
  , seconds
) where

data TimeStamp = TimeStamp {
    sec :: {-# UNPACK #-}!Int
  , nsec :: {-# UNPACK #-}!Int
  } deriving (Eq, Read, Show)

seconds :: TimeStamp -> Double
seconds t = fromIntegral (sec t) + 1e-9 * fromIntegral (nsec t)
