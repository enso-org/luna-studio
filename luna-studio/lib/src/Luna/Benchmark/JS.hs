{-# LANGUAGE CPP #-}
module Luna.Benchmark.JS where

import           Common.Prelude
import qualified Data.Map                as Map
import qualified Debug.Safe              as Safe
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as Topic

import Common.Action.Command (Command)
import Data.Map              (Map)
import Data.Time.Clock       (UTCTime)
import Data.Time.Clock       (diffUTCTime, getCurrentTime)
import Data.UUID.Types       (UUID)

class HasRequestTimes st where
    requestTimes :: Lens' st (Map UUID UTCTime)

measureResponseTime
    :: (HasRequestTimes s, Topic.MessageTopic (Response.Response req inv res))
    => Response.Response req inv res -> Command s ()
#ifdef DEBUG_PERF
measureResponseTime resp = do
    let uuid = resp ^. Response.requestId
    reqTimeM <- use $ requestTimes . at uuid
    liftIO $ case reqTimeM of
        Just reqTime -> do
            currTime <- getCurrentTime
            let timeDiff = show $ diffUTCTime currTime reqTime
            putStrLn $ "[Request time -- NodeEditor] " <> Topic.topic' resp <> " took " <> timeDiff
        Nothing      -> putStrLn $ "[Request time -- NodeEditor] request uuid doesn't match any known requests."
#else
measureResponseTime = const $ pure ()
{-# INLINE measureResponseTime #-}
#endif

foreign import javascript safe "window.performance.now()"
    jsPerformanceNow :: IO Double

performanceNow :: MonadIO m => m Double
performanceNow = liftIO jsPerformanceNow

timeAction :: MonadIO m => String -> m a -> m a
#ifdef DEBUG_PERF
timeAction actName act = withLevel $ \l -> do
    startTime <- performanceNow
    result    <- act
    endTime   <- performanceNow
    let timeDiff = endTime - startTime
    putStrLn $ replicate l ' ' <> actName <> " took: " <> show timeDiff <> "ms"
    pure result
#else
timeAction = const id
#endif
{-# INLINE timeAction #-}

