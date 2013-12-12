module DSP.Artery.Filter.Moog (moogLP, module DSP.Artery.Filter.Types) where

import Control.Artery
import Data.Reflection
import DSP.Artery.Types
import DSP.Artery.Filter.Types

-- | http://musicdsp.org/archive.php?classid=3#26
moogLP :: Fractional a => Artery m (a, FilterParam a) a
moogLP = go 0 0 0 0 0 0 0 0 where
    go x0 x1 x2 x3 y0 y1 y2 y3 = Artery $ \(x_, FilterParam freq res) cont -> do
        let f = freq * 1.16
        let fb = res * (1.0 - 0.15 * f * f)
        let x = (x_ - y3 * fb) * f^4 * 0.35013
        let o0 = x + 0.3 * x0 + (1 - f) * y0
        let o1 = o0 + 0.3 * x1 + (1 - f) * y1
        let o2 = o1 + 0.3 * x2 + (1 - f) * y2
        let o3 = o2 + 0.3 * x3 + (1 - f) * y3
        cont o3 $ go x x0 x1 x2 o0 o1 o2 o3 