{-# LANGUAGE BangPatterns, FlexibleContexts, Rank2Types, DeriveFunctor, Arrows #-}
import DSP.Artery.IO
import DSP.Artery.Oscillator
import DSP.Artery.Filter.Moog as Moog
import DSP.Artery.Envelope
import Control.Artery
import Control.Applicative
import Control.Monad
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
    go i m (Free (Wait d cont)) = w (IM.toList m) (floor $ fromIntegral samplePerBeat * d) where
        w vs 0 = go i (IM.fromAscList vs) cont
        w vs n = Artery $ \_ c -> update vs $ \rs vs' -> c rs (w vs' (n - 1))
    go i m (Pure a) = absurd a
    update ((i, v):vs) cont = unArtery v () $ \z v' -> update vs $ \zs vs' -> cont (z : zs) ((i, v') : vs')
    update [] cont = cont [] []

bpm = 140

samplePerBeat :: Given SampleRate => Int
samplePerBeat = floor $ 60 / bpm * theSampleRate

rhythm :: Given SampleRate => String -> Artery m () Bool
rhythm = fromList . cycle . go where
    n32 = samplePerBeat `div` 8
    n16 = samplePerBeat `div` 4
    go ('^':'^':xs) = replicate n16 True ++ go ('^':xs)
    go ('^':x:xs) = replicate n32 True ++ replicate n32 False ++ go (x:xs)
    go ('*':'^':xs) = replicate n16 True ++ go ('^' : xs)
    go ('*':x:xs) = replicate n32 True ++ replicate n32 False ++ go (x:xs)
    go ('-':xs) = replicate n16 False ++ go xs
    go "*" = replicate n16 True
    go [] = []

melody :: Given SampleRate => Int -> Float -> [Int] -> Artery m () Float
melody d t = fromList
    . concatMap (replicate $ samplePerBeat `div` fromIntegral d)
    . map (\x -> 220 * 2 ** ((fromIntegral x + t) / 12))

saturator :: (Floating a) => a -> Artery m a a
saturator gain = arr $ \x -> 2 / (1 + exp (gain * x)) - 1

pingpong :: Given SampleRate => Artery m (V2 Float) (V2 Float)
pingpong = feedback 0 $ proc (V2 x y, V2 l r) -> do
    dl <- delay (floor $ fromIntegral samplePerBeat * 1.02) 0 -< r + x * 0.8
    dr <- delay (floor $ fromIntegral samplePerBeat * 0.98) 0 -< l + y * 0.2
    let z = V2 dl dr
    returnA -< (V2 x y + z * 0.5, z * 0.7)

stereo :: Artery m a a -> Artery m a a -> Artery m (V2 a) (V2 a)
stereo a b = Artery
    $ \(V2 x y) cont -> unArtery a x
    $ \l contl -> unArtery b y
    $ \r contr -> cont (V2 l r) (stereo contl contr)

stereo' :: Artery m a a -> Artery m (V2 a) (V2 a)
stereo' a = stereo a a

main = withStream def { bufferSize = Just 4096 } (withSampleRate
        $ runPlaylist song >>> arr sum >>> stereo' (saturator 1))
    $ threadDelay (60 * 60 * 1000 * 1000) where
    song :: Given SampleRate => Playlist (V2 Float) a
    song = do
        i <- start intro
        wait (4 * 2)
        stop i
        forever $ do
            i <- start bassline
            j <- start mainMelody
            wait (4 * 8)
            stop i
            stop j

type Synth a = Given SampleRate => Artery m (Float, Bool) a

bell :: Synth Float
bell = proc (freq, gate) -> do
    env <- genADSR 0.01 0.4 0.2 0.4 -< gate
    m <- sineWave -< (64 * freq, 0)
    sineWave -< (freq * 2, m * env * 0.5)

bass :: Synth Float
bass = proc (freq, gate) -> do
    w <- sawWave -< (freq, 0)
    env <- genADSR 0.001 2 0 1 -< gate
    Moog.lowpass -< (w, FilterParam (env * 0.8) 4)

bundle :: Num b => [Artery m a b] -> Artery m a b
bundle xs = Artery $ \i cont -> go i xs $ \o rs -> cont o (bundle rs) where
    go i (v:vs) cont = unArtery v i $ \o r -> go i vs $ \o' rs -> let !z = o + o' in cont z (r : rs)
    go _ [] cont = cont 0 []

harmony :: Given SampleRate => [(Float, Float)] -> Artery m (Float, Float) Float
harmony hs = bundle [arr (first (k*)) >>> sineWave >>> arr (*g) | (k, g) <- hs]

lead :: Synth Float
lead = proc (freq, gate) -> do
    vib <- sineWave -< (3, 0)
    s <- harmony [(1, 1), (2, 0.9), (3, 0.05), (4, 0.25), (5, 0.35)
        , (6, 0.4) , (7, 0.1) , (8, 0.08) , (9, 0.008) , (10, 0.001)
        , (11, 0.02), (14, 0.009), (16, 0.05), (18, 0.004), (20, 0.003)
        , (22, 0.002), (24, 0.001), (26, 0.001)]
        -< (freq * (1 + vib * 0.005), 0)
    env <- genADSR 0.1 2 1 1 -< gate
    Moog.lowpass -< (s, FilterParam (env * 0.9) 1)

intro :: Given SampleRate => Artery m () (V2 Float)
intro = melody 4 12 [4, 5, 7, 7, 12, 12, 4, 5, 7, 12, 14, 16, 14, 11, 12, 12,
                     7, 7, 4, 5, 7, 7, 12, 12, 14, 11, 12, 14, 17, 16, 17, 14]
        &&& rhythm "***-*-*********-*-**-*-*-********"
        >>> bell
        >>> arr pure

mainMelody :: Given SampleRate => Artery m () (V2 Float)
mainMelody = melody 4 12 [
    7, 7, 9, 9, 2, 4, 4, 0, 3, 2, 0, 0, 0, 0, 2, 2,
    3, 3, 4, 2, 0, 2, 4, 7, 9, 4, 7, 2, 3, 0, 2, 0,
    4, 4, 7, 7, 9, 4, 7, 2, 4, 0, 1, 4, 3, 2, 0, 2,
    3, 3, 0, 2, 4, 7, 2, 3, 2, 0, 2, 2, 0, 0, 2, 2,
    0, 0, -5, -3, 0, 0, -5, -3, 0, 2, 4, 0, 5, 4, 5, 7,
    0, 0, 0, 0, -5, -3, 0, -5, 5, 4, 2, 0, -5, -8, -7, -5,
    0, 0, -5, -3, 0, 0, -5, -3, 0, 0, 2, 4, 0, -5, -3, -5,
    0, 0, 0, -1, 0, -5, -3, 0, 5, 4, 5, 7, 0, 0, -1, -1]
    &&& rhythm "*^*^**-****-*^*^\
               \*-**************\
               \*^*^************\
               \*-*********-*^*^\
               \*^***^**********\
               \*^*^************\
               \*^***^**********\
               \*-***********-*-"
    >>> lead
    >>> arr pure
    >>> pingpong

bassline :: Given SampleRate => Artery m () (V2 Float)
bassline = melody 2 (-12) [
    -4, 8, -2, 10, -5, 7, 0, 12,
    -7, 5, -2, 10, -9, 3, -7, 5,
    -4, 8, -2, 10, -5, 7, 0, 12,
    -7, 5, -2, 10, -9, 3, -9, 3]
    &&& rhythm "*-*-"
    >>> bass
    >>> saturator 8
    >>> arr pure
