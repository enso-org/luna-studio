{-# LANGUAGE CPP #-}
module Debug.Safe
( (<!!>)
, timeIt
) where

import           Prologue

#ifdef DEBUG_PERF
import           Control.Concurrent
import           Data.IORef         (IORef)
import qualified Data.IORef         as IORef
import           Data.Time.Clock    (diffUTCTime, getCurrentTime)
import           System.CPUTime     (getCPUTime)
import           System.IO.Unsafe   (unsafePerformIO)


{-# NOINLINE levelRef #-}
levelRef :: IORef Int
levelRef = unsafePerformIO $ IORef.newIORef 0

withLevel :: MonadIO m => (Int -> m a) -> m a
withLevel action = do
    level <- liftIO $ IORef.readIORef levelRef
    liftIO $ IORef.writeIORef levelRef $ level + 1
    r <- action level
    liftIO $ IORef.writeIORef levelRef level
    pure r
#endif

{-# INLINE timeIt #-}
timeIt :: MonadIO m => String -> m a -> m a
#ifdef DEBUG_PERF
timeIt name action = withLevel $ \l -> do
    let spaces = replicate l ' '
    putStrLn $ spaces <> ">> " <> name
    cpuStart <- liftIO getCPUTime
    start    <- liftIO getCurrentTime
    r <- action
    cpuEnd <- liftIO getCPUTime
    end    <- liftIO getCurrentTime
    putStrLn $ spaces <> "<< " <> name
            <> " CPU " <> show ((cpuEnd - cpuStart) `div` 1000000000) <> "ms"
            <> " Wall " <> show (diffUTCTime end start)
    pure r
#else
timeIt _ = id
#endif

{-# INLINE (<!!>) #-}
infixl 0 <!!>
(<!!>) :: MonadIO m => m a -> String -> m a
(<!!>) = flip timeIt
