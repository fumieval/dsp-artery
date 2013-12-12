{-# LANGUAGE FlexibleContexts #-}
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

filterEnv :: Given SampleRate => Artery m Bool (FilterParam Float)
filterEnv = genADSR 0.01 4 0 1 >>> arr (\v -> FilterParam (v * 0.7) 5)

gen :: Given SampleRate => Artery m () Bool -> Artery m () Float ->Artery m () (V2 Float)
gen rhy mel = (mel >>> sawWave) &&& (rhy >>> filterEnv)
    >>> moogLP
    >>> arr pure

rhythm :: Given SampleRate => String -> Artery m () Bool
rhythm = gen16 . map (=='*')

melody :: Given SampleRate => [Int] -> Artery m () Float
melody = gen16 . map (\x -> 110 * 2 ** (fromIntegral x / 12))

gen16 :: Given SampleRate => [a] -> Artery m () a
gen16 = fromList . concatMap (replicate $ floor $ 60 / bpm * theSampleRate / 4)

main = withStream def (withSampleRate $ gen (rhythm r) (melody m)) $ threadDelay (10 * 1000 * 1000) where
    r = "*-*-*-*-*-*-*-*-"
    m = [-4, -4, 8, 8, -2, -2, 10, 10, -5, -5, 7, 7, 0, 0, 12, 12]