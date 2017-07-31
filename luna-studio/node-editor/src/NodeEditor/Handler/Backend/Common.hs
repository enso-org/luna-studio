module NodeEditor.Handler.Backend.Common
    ( whenOk
    , handleResponse
    , doNothing
    ) where

import           Common.Action.Command   (Command)
import           Common.Debug            (measureResponseTime)
import           Common.Prelude
import qualified Data.Aeson              as JSON (ToJSON)
import qualified Data.Map                as Map
import           Data.Time.Clock         (diffUTCTime, getCurrentTime)
import qualified Data.UUID.Types         as UUID (toString)
import qualified JS.Debug                as Debug
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as Topic
import           NodeEditor.Action.UUID  (isOwnRequest, unregisterRequest)
import           NodeEditor.State.Global (State, backend, pendingRequests)

whenOk :: Response.Response req inv res -> (res -> Command State ()) -> Command State ()
whenOk (Response.Response _ _ _ _ (Response.Ok    res)) handler = handler res
whenOk (Response.Response _ _ _ _ (Response.Error _  )) _       = return ()

handleResponse :: (Topic.MessageTopic (Response.Response req inv res), JSON.ToJSON req) => Response.Response req inv res -> (res -> Command State ()) -> (Response.Status inv -> Command State ()) -> Command State ()
handleResponse resp@(Response.Response uuid _ req inv res) success failure = do
    case res of
        Response.Ok    res' -> success res'
        Response.Error str  -> do
            liftIO $ Debug.error (convert $ Topic.topic resp <> " [" <> UUID.toString uuid <> "] " <> str) req
            failure inv
    measureResponseTime resp
    whenM (isOwnRequest uuid) $ unregisterRequest uuid

doNothing :: a -> Command State ()
doNothing = const $ return ()
