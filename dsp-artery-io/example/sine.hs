import DSP.Artery.IO
import DSP.Artery.Oscillator
import Control.Artery
import Control.Arrow
import Control.Applicative
import Control.Concurrent

main = withStream def (pure 440 >>> sineWave >>> arr pure) $ threadDelay (10 * 1000 * 1000)