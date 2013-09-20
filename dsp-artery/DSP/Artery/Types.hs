module DSP.Artery.Types (SampleRate(..), getSampleRate) where

newtype SampleRate = SampleRate Int

getSampleRate :: Num a => SampleRate -> a
getSampleRate (SampleRate a) = fromIntegral a
{-# INLINE getSampleRate #-}