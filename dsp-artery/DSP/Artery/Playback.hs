module DSP.Artery.Playback where
import Control.Concurrent
import Control.Artery
import Data.IntMap.Strict as IM
import Data.IORef
import Control.Applicative
import Control.Monad

play :: Num a => Buffer a -> [a] -> IO Voice
play (Buffer mv mi) s = do
    i <- modifyMVar mi (\a -> return (a + 1, a))
    modifyMVar_ mv (return . IM.insert i s)
    return (Voice i)

stop :: Buffer a -> Voice -> IO ()
stop (Buffer mv _) (Voice i) = modifyMVar_ mv (return . IM.delete i)

playbackChunked :: Num a => Int -> Buffer a -> Artery IO Int [a]
playbackChunked n (Buffer mv _) = effectful $ \i -> do
    im <- takeMVar mv
    r <- newIORef $ replicate n 0
    m <- forM (IM.toAscList im) $ \(i, cs) -> let (a, b) = splitAt n cs in (i, b) <$ modifyIORef' r (zipWith (+) a)
    putMVar mv $ IM.fromAscList m
    readIORef r

newBuffer :: IO (Buffer a)
newBuffer = Buffer <$> newMVar IM.empty <*> newMVar 0

data Buffer a = Buffer (MVar (IM.IntMap [a])) (MVar Int)

data Voice = Voice Int
