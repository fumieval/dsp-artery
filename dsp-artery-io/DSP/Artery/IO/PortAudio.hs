{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module DSP.Artery.IO.PortAudio (DeviceSettings(..), withStream) where

import Control.Artery
import qualified Sound.PortAudio as PA
import qualified Sound.PortAudio.Base as PA
import Foreign.Storable
import Data.IORef
import Control.Monad
import Control.Concurrent
import Foreign.Ptr
import Foreign.C.Types
import Linear
import DSP.Artery.IO.Types
import Foreign.Marshal.Array

audioCallback :: IORef (Artery IO Int [V2 CFloat]) -> PA.StreamCallback CFloat CFloat
audioCallback ref _ _ frames _in _out = do
    ar <- readIORef ref
    unArtery ar (fromEnum frames) $ \o cont -> do
        pokeArray (castPtr _out) o
        writeIORef ref cont
        return PA.Continue

withStream :: DeviceSettings -> Artery IO Int [V2 CFloat] -> IO a -> IO a
withStream setting ar m = do
    ref <- newIORef ar
    res <- PA.withPortAudio $ PA.withStream
        Nothing
        (fmap (\i -> PA.StreamParameters (PA.PaDeviceIndex $ fromIntegral i) 2 (PA.PaTime 0.0)) (deviceId setting))
        (fromIntegral $ sampleRate setting)
        (bufferSize setting)
        []
        (Just $ audioCallback ref)
        (Just $ return ())
        $ \stream -> do
            _ <- PA.startStream stream
            r <- m
            _ <- PA.stopStream stream
            return $ Right r
    either (fail . show) return res