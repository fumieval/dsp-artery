{-# LANGUAGE Rank2Types, FlexibleContexts #-}
module DSP.Artery.IO.DirectSound (withStream) where

import Control.Monad
import Control.Concurrent
import Control.Artery
import Data.Maybe
import Data.IORef
import qualified Sound.Win32.DirectSound as DS
import DSP.Artery.IO.Types
import Linear
import Foreign

audioCallback :: IORef (Artery IO () (V2 Float)) -> Ptr Int16 -> Word32 -> IO ()
audioCallback ref buf frames = readIORef ref >>= write 0 >>= writeIORef ref where
    out :: Ptr (V2 Int16)
    out = castPtr buf

    write i ar
        | i == fromEnum frames = return ar
        | otherwise = do
            unArtery ar () $ \o cont -> pokeElemOff out i (fmap (floor . (*32768)) o)
                >> write (succ i) cont

withStream :: DeviceSettings -> (Int -> Artery IO () (V2 Float)) -> IO a -> IO a
withStream param ar m = do
    devs <- DS.enumerateDrivers
    drv <- case devs of
        [] -> fail "No audio device"
        [x] -> return x
        (d:_) -> return $ maybe d (devs !!) (deviceId param)

    hwnd <- DS.getConsoleHWND_hack

    ds <- DS.directSoundCreate (Just drv) hwnd >>= \mds -> case mds of
        Left err -> fail err
        Right ds -> return ds
    let waveFormatX = DS.makeWaveFormatX (sampleRate param) 2 DS.SampleInt16
    
    buf <- DS.createSoundBuffer ds waveFormatX (2 * maybe 2048 id (bufferSize param)) >>= \msb -> case msb of
        Left err -> fail err
        Right sb -> return sb

    ref <- newIORef $ ar $ sampleRate param

    stop <- DS.playWithDoubleBuffering buf (audioCallback ref)
    r <- m
    stop
    return r