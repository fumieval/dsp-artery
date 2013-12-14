{-# LANGUAGE FlexibleContexts #-}
module DSP.Artery.Oscillator where

import Control.Artery
import Data.Reflection
import DSP.Artery.Types
import qualified Data.Vector as V

ring :: V.Vector a -> Artery m (Float, Float) a
ring v = go 0 where
    n = fromIntegral (V.length v)
    go i
        | i >= n = go $ i - n
        | otherwise = Artery $ \(rate, phase) cont -> let p = i + phase / (2 * pi)
            in cont (v V.! (floor p `mod` V.length v)) (go (i + rate))

sineWave :: (Floating a, Ord a, Given SampleRate) => Artery m (a, a) a
sineWave = go 0 where
    k = 2 * pi / theSampleRate
    go i
        | i > 2 * pi = go $! i - 2 * pi
        | otherwise = Artery $ \(freq, phase) cont -> cont (sin $! i + phase) (go $! i + k * freq)
    {-# INLINE go #-}

sawWave :: (Given SampleRate, Fractional a) => Artery m (Float, Float) a
sawWave = ring $ V.iterateN theSampleRate ((2/theSampleRate)+) (-1)

squareWave :: (Given SampleRate, Num a) => Artery m (Float, Float) a
squareWave = let n = theSampleRate `div` 2
    in ring $ V.fromList $ replicate n (-1) ++ replicate n 1 

triangleWave :: (Given SampleRate, Fractional a) => Artery m (Float, Float) a
triangleWave = let n = theSampleRate `div` 2
    in ring $ V.fromList $ map ((/fromInteger n) . fromInteger) [0..n-1] ++ map ((1-) . (/fromInteger n) . fromInteger) [0..n-1]