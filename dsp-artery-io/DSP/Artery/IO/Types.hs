module DSP.Artery.IO.Types (Backend(..), DeviceSettings(..)) where
import Data.Default

data Backend = PortAudio | DirectSound

data DeviceSettings = DeviceSettings
    { preferredBackend :: Backend
    , sampleRate :: Int
    , deviceId :: Maybe Int
    , bufferSize :: Maybe Int
    }

instance Default DeviceSettings where
    def = DeviceSettings
        { preferredBackend = DirectSound
        , sampleRate = 44100
        , deviceId = Just 0
        , bufferSize = Nothing
        }
