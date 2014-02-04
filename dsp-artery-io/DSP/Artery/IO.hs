{-# LANGUAGE CPP, Rank2Types, FlexibleContexts #-}
module DSP.Artery.IO(withStream, DeviceSettings(..), def, Backend(..)) where

import DSP.Artery.IO.Types
import Linear
import Control.Artery
import Data.Default
import Data.WAVE
import Data.IORef
import Foreign.C.Types

#ifdef DSOUND
import qualified DSP.Artery.IO.DirectSound as DSound
#endif

#ifdef PORTAUDIO
import qualified DSP.Artery.IO.PortAudio as PortAudio
#endif

withStream :: DeviceSettings -> Artery IO Int [V2 CFloat] -> IO a -> IO a

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
