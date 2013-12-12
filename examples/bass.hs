{-# LANGUAGE FlexibleContexts, Rank2Types #-}
import DSP.Artery.IO
import DSP.Artery.Oscillator
import DSP.Artery.Filter.Moog
import DSP.Artery.Envelope
import Control.Artery
import Control.Arrow
import Control.Applicative
import Control.Concurrent
import DSP.Artery.Types
import Data.Reflection
import Linear

bpm = 140

rhythm :: Given SampleRate => String -> Artery m () Bool
rhythm = gen16 . map (=='*')

melody :: Given SampleRate => [Int] -> Artery m () Float
melody = gen16 . map (\x -> 110 * 2 ** (fromIntegral x / 12))

gen16 :: Given SampleRate => [a] -> Artery m () a
gen16 = fromList . concatMap (replicate $ floor $ 60 / bpm * theSampleRate / 4)

saturator :: (Floating a) => a -> Artery m a a
saturator gain = arr $ \x -> 2 / (1 + exp (gain * x)) - 1

stereo :: Artery m a a -> Artery m a a -> Artery m (V2 a) (V2 a)
stereo a b = Artery
    $ \(V2 x y) cont -> unArtery a x
    $ \l contl -> unArtery b y
    $ \r contr -> cont (V2 l r) (stereo contl contr)

stereo' :: Artery m a a -> Artery m (V2 a) (V2 a)
stereo' a = stereo a a

main = withStream def (withSampleRate $ bassline >>> stereo' (saturator 20)) $ threadDelay (10 * 1000 * 1000) where

bassline :: Given SampleRate => Artery m () (V2 Float)
bassline = melody [-4, -4, 8, 8, -2, -2, 10, 10, -5, -5, 7, 7, 0, 0, 12, 12]
    &&& rhythm "*-*-*-*-*-*-*-*-"
    >>> sawWave *** fltEnv
    >>> moogLP
    >>> arr pure
    where
        fltEnv = genADSR 0.01 3 0 1 >>> arr (\v -> FilterParam (v * 0.8) 4)
