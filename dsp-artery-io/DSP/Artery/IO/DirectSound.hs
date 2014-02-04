{-# LANGUAGE Rank2Types, FlexibleContexts #-}
module DSP.Artery.IO.DirectSound (withStream) where

import Control.Monad
import Control.Concurrent
import Data.IORef
import Control.Artery
import Data.Maybe
import qualified Sound.Win32.DirectSound as DS
import DSP.Artery.IO.Types
import Linear
import Foreign
import Foreign.C.Types
import Foreign.Marshal.Array
import Data.IORef

audioCallback :: IORef (Artery IO Int [V2 CFloat]) -> Ptr (V2 Int16) -> Word32 -> IO ()
audioCallback ref buf frames = do
    ar <- readIORef ref
    unArtery ar (fromEnum frames) $ \o cont -> do
        pokeArray buf $ map (fmap (floor . (*32768))) o
        writeIORef ref cont

withStream :: DeviceSettings -> Artery IO Int [V2 CFloat] -> IO a -> IO a
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
    let bufSize = maybe 2048 id (bufferSize param)
    buf <- DS.createSoundBuffer ds waveFormatX (2 * bufSize) >>= either fail return
    ref <- newIORef ar
    stop <- DS.playWithDoubleBuffering buf (audioCallback ref)
    r <- m
    stop
    return r
