{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module DSP.Artery.PortAudio (DeviceSettings(..), withStream) where

import Control.Artery
import qualified Sound.PortAudio as PA
import Sound.PortAudio.Base
import Foreign.Storable
import Foreign.C.Types
import Data.IORef
import Control.Monad
import Control.Concurrent
import Foreign.Ptr
import Linear
import Data.Default
import Data.Reflection
import DSP.Artery.Types

audioCallback :: IORef (Artery IO (V2 CFloat) (V2 CFloat)) -> PA.StreamCallback CFloat CFloat
audioCallback ref _ _ frames _in _out = do
    readIORef ref >>= write 0 >>= writeIORef ref
    return PA.Continue
    where
        in_ :: Ptr (V2 CFloat)
        in_ = castPtr _in
        out :: Ptr (V2 CFloat)
        out = castPtr _out

        write i ar
            | i == fromEnum frames = return ar
            | otherwise = do
                v <- peekElemOff in_ i
                unArtery ar v $ \o cont -> pokeElemOff out i o
                    >> write (succ i) cont

data DeviceSettings = DeviceSettings {
    _sampleRate :: Int
    , _inputDeviceId :: Maybe Int
    , _outputDeviceId :: Maybe Int
    , _bufferSize :: Maybe Int
    }

instance Default DeviceSettings where
    def = DeviceSettings
        { _sampleRate = 44100
        , _inputDeviceId = Nothing
        , _outputDeviceId = Just 0
        , _bufferSize = Nothing
        }

withStream :: DeviceSettings -> (Given SampleRate => Artery IO (V2 CFloat) (V2 CFloat)) -> IO a -> IO (Either PA.Error a)
withStream setting ar m = do
    ref <- newIORef $ give (SampleRate $ _sampleRate setting) ar
    PA.withPortAudio $ PA.withStream
        (fmap (\i -> PA.StreamParameters (PaDeviceIndex $ fromIntegral i) 2 (PaTime 0.0)) (_inputDeviceId setting))
        (fmap (\i -> PA.StreamParameters (PaDeviceIndex $ fromIntegral i) 2 (PaTime 0.0)) (_outputDeviceId setting))
        (fromIntegral $ _sampleRate setting)
        (_bufferSize setting)
        []
        (Just $ audioCallback ref)
        (Just $ return ())
        $ \stream -> do
            _ <- PA.startStream stream
            r <- m
            _ <- PA.stopStream stream
            return $ Right r