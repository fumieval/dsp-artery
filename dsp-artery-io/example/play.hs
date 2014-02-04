import DSP.Artery.IO
import Control.Concurrent.MVar
import Control.Concurrent
import Data.WAVE
import Linear
import Control.Arrow
import Foreign.C.Types

readWave :: FilePath -> IO [V2 CFloat]
readWave path = do
    WAVE _ ss <- getWAVEFile path
    return [case s of {[l, r] -> V2 (f l) (f r); [v] -> V2 (f v) (f v)} | s <- ss]
    where

        maxb = fromIntegral (maxBound :: WAVESample)
        minb = fromIntegral (minBound :: WAVESample)
        f v
            | v >= 0 = fromIntegral v / maxb
            | otherwise = -(fromIntegral v) / minb

main = do
    v <- newMVar returnA
    w <- readWave "horn.wav"
    withStream def v $ \_ play -> do
        play w
        threadDelay $ 10 * 1000000