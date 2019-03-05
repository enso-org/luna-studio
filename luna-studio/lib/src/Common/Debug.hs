{-# LANGUAGE CPP #-}
module Common.Debug where

import           Common.Action.Command
import           Common.Prelude
import           Data.Map                (Map)
import           Data.Time.Clock         (UTCTime)
import           Data.UUID.Types         (UUID)
import           Debug.Safe              (withLevel)
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as Topic
#ifdef DEBUG_PERF
import qualified Data.Map                as Map
import           Data.Time.Clock         (diffUTCTime, getCurrentTime)
#endif

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
measureResponseTime _ = return ()
#endif

foreign import javascript safe "window.performance.now()"
    performanceNow :: IO Double

timeAction :: MonadIO m => String -> m a -> m a
#ifdef DEBUG_PERF
timeAction actName act = withLevel $ \l -> do
    startTime <- liftIO performanceNow
    result    <- act
    endTime   <- liftIO performanceNow
    let timeDiff = endTime - startTime
    putStrLn $ replicate l ' ' <> actName <> " took: " <> show timeDiff <> "ms"
    pure result
#else
timeAction _ = id
#endif

