module DSP.Artery.Oscillator where

import Control.Artery
import Data.Reflection
import DSP.Artery.Types
import Data.Vector as V
data WaveTable a = WaveTable (V.Vector a)

ring :: V.Vector a -> Artery m Float a
ring v = go 0 where
    go i
        | floor i >= V.length v = go $ i - fromIntegral (V.length v)
        | otherwise = Artery $ \rate cont -> cont (v Vector.! floor i) (go (i + rate))

sineWave :: (Fractional a, Given SampleRate) => Artery m a a
sineWave = go 0 where
    k = 2 * pi / getSampleRate given
    go i
        | i > 2 * pi = go (i - 2 * pi)
        | otherwise = Artery $ \freq cont -> cont (sin i) (go (i + k * freq))

sawWave :: (Given SampleRate) => Artery m Float a
sawWave = ring $ V.iterateN (getSampleRate given) ((2/getSampleRate given)+) (-1)

squareWave :: (Given SampleRate) => Artery m Float a
squareWave = let n = getSampleRate given `div` 2
    in ring $ V.fromList $ replicate n (-1) ++ replicate n 1 

triangleWave :: (Given SampleRate) => Artery m Float a
triangleWave = ring $ V.fromList $ map (/n) [0..n-1] ++ map ((1-) . (/n)) [0..n-1] where
    g = ((4/getSampleRate given)+)
    n = getSampleRate given / 2
