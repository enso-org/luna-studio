module Debug.Unsafe where

import           Data.Time.Clock  (diffUTCTime, getCurrentTime)
import           Prologue
import           System.CPUTime
import           System.IO.Unsafe


debugPrint :: (Show a, Monad m) => a -> m ()
debugPrint = unsafeLiftIO . print

debugPPrint :: (Show a, Monad m) => a -> m ()
debugPPrint = unsafeLiftIO . pprint

debugPutStrLn :: Monad m => String -> m ()
debugPutStrLn = unsafeLiftIO . putStrLn

unsafeLiftIO :: Monad m => IO a -> m a
unsafeLiftIO action = do
    let r = unsafePerformIO action
    r `seq` pure r

timeIt' :: Monad m => String -> m a -> m a
timeIt' name action = do
    (cpuStart, start) <- unsafeLiftIO $ do
        putStrLn $ ">> " <> name
        (,) <$> getCPUTime <*> getCurrentTime
    r <- action

    r `seq` unsafeLiftIO $ do
        cpuEnd <- getCPUTime
        end    <- getCurrentTime
        let cpuVal = show ((cpuEnd - cpuStart) `div` 1000000000) <> "ms"
        putStrLn $ "<< "    <> name
                <> " CPU "  <> cpuVal
                <> " Wall " <> show (diffUTCTime end start)
    pure r
