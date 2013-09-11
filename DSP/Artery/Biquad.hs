{-# LANGUAGE Rank2Types, DeriveFunctor, FlexibleContexts #-}
module DSP.Artery.Biquad where

import Control.Artery
import Data.Reflection
import DSP.Types

data FilterParam a = FilterParam
    { _filterCutOff :: a
    , _filterQ :: a
    }

data BiquadParam a = BiquadParam !a !a !a !a !a !a deriving (Show, Read, Eq, Ord, Functor)

biquadHelper :: (Floating a, Given SampleRate) => (a -> a -> BiquadParam a) -> FilterParam a -> BiquadParam a
biquadHelper f (FilterParam freq q) = let omega = 2 * pi * freq / getSampleRate given in f omega (sin omega / q / 2)
{-# INLINE biquadHelper #-} 

biquadFilter :: Fractional a => Artery m (a, BiquadParam a) a
biquadFilter = go 0 0 0 0 where
    go x' x'' y' y'' = Artery $ \(x, BiquadParam a0 a1 a2 b0 b1 b2) cont -> do
        let y = (b0 * x + b1 * x' + b2 * x'' - a1 * y' - a2 * y'') / a0
        cont y $ go x x' y y'
{-# INLINE biquadFilter #-}

lowpass :: (Floating a, Given SampleRate) => FilterParam a -> BiquadParam a
lowpass = biquadHelper (\w a -> let v = 1 - cos w in BiquadParam (1 + a) (-2 * cos w) (1 - a) (v / 2) v (v / 2))

highpass :: (Floating a, Given SampleRate) => FilterParam a -> BiquadParam a
highpass = biquadHelper $ \w a -> let v = 1 + cos w in BiquadParam (1 + a) (-2 * cos w) (1 - a) (v / 2) v (v / 2)

bandpass :: (Floating a, Given SampleRate) => FilterParam a -> BiquadParam a
bandpass = biquadHelper $ \w a -> BiquadParam (1 + a) (-2 * cos w) (1 - a) (sin w / 2) 0 (sin w / 2)

notch :: (Floating a, Given SampleRate) => FilterParam a -> BiquadParam a
notch = biquadHelper $ \w a -> BiquadParam (1 + a) (-2 * cos w) (1 - a) 1 (-2 * cos w) 1

allpass :: (Floating a, Given SampleRate) => FilterParam a -> BiquadParam a
allpass = biquadHelper $ \w a -> BiquadParam (1 + a) (-2 * cos w) (1 - a) (1 - a) (-2 * cos w) (1 + a)

peaking :: (Floating a, Given SampleRate) => a -> FilterParam a -> BiquadParam a
peaking gain = biquadHelper $ \w a -> BiquadParam (1 + a * g) (-2 * cos w) (1 - a * g)
    (1 + a * g) (-2 * cos w) (1 - a * g) where g = sqrt $ 10 ** (gain / 20)
