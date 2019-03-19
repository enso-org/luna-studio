-- TODO: According to WD, this module should be removed. See here: https://github.com/luna/luna-studio/pull/1349#discussion_r266744449
{-# LANGUAGE CPP #-}

module Debug.Safe where

import Prologue hiding (print, printLn, putStrLn)

import qualified Data.IORef as IORef

import Control.Concurrent
import Data.IORef         (IORef)
import Data.Time.Clock    (diffUTCTime, getCurrentTime)
import Debug.Console
import System.CPUTime     (getCPUTime)
import System.IO.Unsafe   (unsafePerformIO)


levelRef :: IORef Int
levelRef = unsafePerformIO $ IORef.newIORef 0
{-# NOINLINE levelRef #-}

withLevel :: MonadIO m => (Int -> m a) -> m a
withLevel action = do
    level <- liftIO $ IORef.readIORef levelRef
    liftIO $ IORef.writeIORef levelRef $ level + 1
    r <- action level
    liftIO $ IORef.writeIORef levelRef level
    pure r

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
