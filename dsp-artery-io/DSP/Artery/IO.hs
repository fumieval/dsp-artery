{-# LANGUAGE CPP, Rank2Types, FlexibleContexts #-}
module DSP.Artery.IO(withStream, DeviceSettings(..), Backend(..)) where

import DSP.Artery.IO.Types
import Linear
import Control.Artery
import Foreign.C.Types
import DSP.Artery.Types
import Data.Reflection

#ifdef D_DSOUND
import DSP.Artery.Device.DirectSound as DSound
#endif

#ifdef D_PORTAUDIO
import qualified DSP.Artery.Device.PortAudio as PortAudio
#endif

withStream :: DeviceSettings -> (Given SampleRate => Artery IO () (V2 CFloat)) -> IO a -> IO a

#ifdef D_DSOUND

#ifdef D_PORTAUDIO
withStream param = case preferredBackend of
    DirectSound -> DSound.withStream param
    PortAudio -> PortAudio.withStream param
#else
withStream = DSound.withStream
#endif

#else

#ifdef D_PORTAUDIO
withStream = DSound.withStream
#else
withStream = error "No backends"
#endif

#endif