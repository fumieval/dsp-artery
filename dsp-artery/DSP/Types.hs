module DSP.Types (SampleRate(..)) where

newtype SampleRate = SampleRate Int

getSampleRate :: Num a => SampleRate -> a
getSampleRate (SampleRate a) = fromIntegral a
{-# INLINE getSampleRate #-}