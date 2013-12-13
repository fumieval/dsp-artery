{-# LANGUAGE FlexibleContexts, Rank2Types, DeriveFunctor #-}
import DSP.Artery.IO
import DSP.Artery.Oscillator
import DSP.Artery.Filter.Moog
import DSP.Artery.Envelope
import Control.Artery
import Control.Arrow
import Control.Applicative
import Control.Concurrent
import DSP.Artery.Types
import Data.Reflection
import Linear
import qualified Data.IntMap as IM
import Data.Void
import Control.Monad.Free

newtype ClipId = ClipId Int

data PlaylistF r a = Start (Artery IO () r) (ClipId -> a)
    | Stop ClipId a
    | Wait Float a
    | Fork a a
    deriving Functor

start :: Artery IO () r -> Playlist r ClipId
start a = wrap $ Start a return

stop :: ClipId -> Playlist r ()
stop i = wrap $ Stop i (return ())

wait :: Float -> Playlist r ()
wait t = wrap $ Wait t (return ())

type Playlist r = Free (PlaylistF r)

runPlaylist :: Given SampleRate => Playlist r Void -> Artery IO () [r]
runPlaylist = go 0 IM.empty where
    go i m (Free (Start a cont)) = go (succ i) (IM.insert i a m) $ cont $ ClipId i
    go i m (Free (Stop (ClipId j) cont)) = go i (IM.delete j m) cont
    go i m (Free (Wait d cont)) = wait (IM.toList m) (floor $ 60 / bpm * theSampleRate * d) where
        wait vs 0 = go i (IM.fromAscList vs) cont
        wait vs n = Artery $ \_ c -> update vs $ \rs vs' -> c rs (wait vs' (n - 1))
    go i m (Free (Fork a b)) = go i m a &&& go i m b >>> arr (uncurry (++))
    go i m (Pure a) = absurd a
    update ((i, v):vs) cont = unArtery v () $ \z v' -> update vs $ \zs vs' -> cont (z : zs) ((i, v') : vs')
    update [] cont = cont [] []

bpm = 140

rhythm :: Given SampleRate => String -> Artery m () Bool
rhythm = gen16 . map (=='*')

melody :: Given SampleRate => [Int] -> Artery m () Float
melody = gen16 . map (\x -> 110 * 2 ** (fromIntegral x / 12))

gen16 :: Given SampleRate => [a] -> Artery m () a
gen16 = fromList . concatMap (replicate $ floor $ 60 / bpm * theSampleRate / 4)

saturator :: (Floating a) => a -> Artery m a a
saturator gain = arr $ \x -> 2 / (1 + exp (gain * x)) - 1

stereo :: Artery m a a -> Artery m a a -> Artery m (V2 a) (V2 a)
stereo a b = Artery
    $ \(V2 x y) cont -> unArtery a x
    $ \l contl -> unArtery b y
    $ \r contr -> cont (V2 l r) (stereo contl contr)

stereo' :: Artery m a a -> Artery m (V2 a) (V2 a)
stereo' a = stereo a a

main = withStream def (withSampleRate $ runPlaylist song >>> arr sum >>> stereo' (saturator 20))
    $ threadDelay (10 * 1000 * 1000) where
    song :: Given SampleRate => Playlist (V2 Float) a
    song = do
        i <- start bassline
        wait 4
        stop i
        song

bassline :: Given SampleRate => Artery m () (V2 Float)
bassline = melody [-4, -4, 8, 8, -2, -2, 10, 10, -5, -5, 7, 7, 0, 0, 12, 12]
    &&& rhythm "*-*-*-*-*-*-*-*-"
    >>> sawWave *** fltEnv
    >>> lowpass
    >>> arr pure
    where
        fltEnv = genADSR 0.01 3 0 1 >>> arr (\v -> FilterParam (v * 0.8) 4)