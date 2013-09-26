{-# LANGUAGE FlexibleContexts #-}
module DSP.Artery.Oscillator where

import Control.Artery
import Data.Reflection
import DSP.Artery.Types
import qualified Data.Vector as V

ring :: V.Vector a -> Artery m Float a
ring v = go 0 where
    go i
        | floor i >= V.length v = go $ i - fromIntegral (V.length v)
        | otherwise = Artery $ \rate cont -> cont (v V.! floor i) (go (i + rate))

sineWave :: (Floating a, Ord a, Given SampleRate) => Artery m a a
sineWave = go 0 where
    k = 2 * pi / getSampleRate given
    go i
        | i > 2 * pi = go (i - 2 * pi)
        | otherwise = Artery $ \freq cont -> cont (sin i) (go (i + k * freq))

sawWave :: (Given SampleRate, Fractional a) => Artery m Float a
sawWave = ring $ V.iterateN (getSampleRate given) ((2/getSampleRate given)+) (-1)

squareWave :: (Given SampleRate, Num a) => Artery m Float a
squareWave = let n = getSampleRate given `div` 2
    in ring $ V.fromList $ replicate n (-1) ++ replicate n 1 

triangleWave :: (Given SampleRate, Fractional a) => Artery m Float a
triangleWave = let n = getSampleRate given `div` 2
    in ring $ V.fromList $ map ((/fromInteger n) . fromInteger) [0..n-1] ++ map ((1-) . (/fromInteger n) . fromInteger) [0..n-1]