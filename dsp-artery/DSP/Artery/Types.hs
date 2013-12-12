{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module DSP.Artery.Types (SampleRate(..), getSampleRate, theSampleRate, withSampleRate) where
import Data.Reflection

newtype SampleRate = SampleRate Int

getSampleRate :: Num a => SampleRate -> a
getSampleRate (SampleRate a) = fromIntegral a
{-# INLINE getSampleRate #-}

theSampleRate :: (Num a, Given SampleRate) => a
theSampleRate = getSampleRate given

withSampleRate :: forall a. (Given SampleRate => a) -> Int -> a
withSampleRate r i = give (SampleRate i) r
{-# INLINE withSampleRate #-}