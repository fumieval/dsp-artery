{-# LANGUAGE CPP, Rank2Types, FlexibleContexts #-}
module DSP.Artery.IO(withStream, DeviceSettings(..), def, Backend(..), exportStream) where

import DSP.Artery.IO.Types
import Linear
import Control.Artery
import Data.Default
import Data.WAVE

#ifdef DSOUND
import qualified DSP.Artery.IO.DirectSound as DSound
#endif

#ifdef PORTAUDIO
import qualified DSP.Artery.IO.PortAudio as PortAudio
#endif

withStream :: DeviceSettings -> (Int -> Artery IO () (V2 Float)) -> IO a -> IO a

#ifdef DSOUND

#ifdef PORTAUDIO
withStream param = case preferredBackend param of
    DirectSound -> DSound.withStream param
    PortAudio -> PortAudio.withStream param
#else
withStream = DSound.withStream
#endif

#else

#ifdef PORTAUDIO
withStream = PortAudio.withStream
#else
withStream = error "No backends"
#endif

#endif

exportStream :: FilePath -> Int -> Int -> (Int -> Artery IO () (V2 Float)) -> IO ()
exportStream path rate len a = go len (a rate) $ putWAVEFile path . WAVE (WAVEHeader 2 rate 24 (Just len)) where
        go :: Int -> Artery IO () (V2 Float) -> ([[WAVESample]] -> IO a) -> IO a
        go 0 _ con = con []
        go n ar con = unArtery ar () $ \(V2 l r) cont -> go (n - 1) cont $ \xs -> con $ [f l, f r] : xs where
            f v =
                let maxb = fromIntegral (maxBound :: WAVESample)
                    minb = fromIntegral (minBound :: WAVESample) in
                if v >= 0
                    then (fromIntegral . floor . (* maxb)) (min v 1)
                    else (fromIntegral . ceiling . (* minb)) (min (-v) 1)