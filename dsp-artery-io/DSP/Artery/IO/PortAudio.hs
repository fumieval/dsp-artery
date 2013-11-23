{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module DSP.Artery.IO.PortAudio (DeviceSettings(..), withStream) where

import Control.Artery
import qualified Sound.PortAudio as PA
import qualified Sound.PortAudio.Base as PA
import Foreign.Storable
import Foreign.C.Types
import Data.IORef
import Control.Monad
import Control.Concurrent
import Foreign.Ptr
import Linear
import Data.Reflection
import DSP.Artery.Types
import DSP.Artery.IO.Types

audioCallback :: IORef (Artery IO () (V2 CFloat)) -> PA.StreamCallback CFloat CFloat
audioCallback ref _ _ frames _in _out = do
    readIORef ref >>= write 0 >>= writeIORef ref
    return PA.Continue
    where
        out :: Ptr (V2 CFloat)
        out = castPtr _out

        write i ar
            | i == fromEnum frames = return ar
            | otherwise = do
                unArtery ar () $ \o cont -> pokeElemOff out i o
                    >> write (succ i) cont

withStream :: DeviceSettings -> (Given SampleRate => Artery IO () (V2 CFloat)) -> IO a -> IO a
withStream setting ar m = do
    ref <- newIORef $ give (SampleRate $ sampleRate setting) ar
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