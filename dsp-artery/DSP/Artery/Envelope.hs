{-# LANGUAGE FlexibleContexts #-}
module DSP.Artery.Envelope (genADSR) where

import Data.Reflection
import Control.Artery
import DSP.Artery.Types

data ADSR = Attack | Decay | Sustain | Release | Idle

genADSR :: (Given SampleRate, Floating a, Ord a) => a -> a -> a -> a -> Artery m Bool a
genADSR tA tD sus tR = go Idle 0 where
    ratio0 = 0.3
    ratio1 = 0.0001
    bA = (1.0 + ratio0) * (1.0 - kA)
    kA = coef ratio0 (theSampleRate * tA)
    bD = (sus - ratio1) * (1.0 - kD)
    kD = coef ratio1 (theSampleRate * tD)
    bR = -ratio1 * (1.0 - kR)
    kR = coef ratio1 (theSampleRate * tR)
    coef ratio rate = exp $ - log ((1.0 + ratio) / ratio) / rate
    go Idle z = Artery $ \i cont -> case i of
        False -> update Idle 0 $ \s' z' -> cont z' (go s' z')
        True -> update Attack 0 $ \s' z' -> cont z' (go s' z')
    go Release z = Artery $ \i cont -> case i of
        False -> update Release z $ \s' z' -> cont z' (go s' z')
        True -> update Attack z $ \s' z' -> cont z' (go s' z')
    go s z = Artery $ \i cont -> case i of
        False -> update Release z $ \s' z' -> cont z' (go s' z')
        True -> update s z $ \s' z' -> cont z' (go s' z')

    update Attack z cont
        | z < 1.0 = cont Attack (min 1 $ bA + z * kA)
        | otherwise = cont Decay 1
    update Decay z cont
        | z > sus = cont Decay (max sus $ bD + z * kD)
        | otherwise = cont Sustain sus
    update Sustain z cont = cont Sustain z
    update Release z cont
        | z > 0 = cont Release (max 0 $ bR + z * kR)
        | otherwise = cont Idle 0
    update Idle z cont = cont Idle z