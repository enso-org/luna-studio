module Luna.Studio.Handler.Backend.Common
    ( whenOk
    , handleResponse
    , doNothing
    ) where

import qualified Data.Aeson                 as JSON (ToJSON)
import qualified Data.UUID.Types            as UUID (toString)
import qualified Empire.API.Response        as Response
import qualified Empire.API.Topic           as Topic
import qualified JS.Debug                   as Debug
import           Luna.Studio.Action.Command (Command)
import           Luna.Studio.Action.UUID    (isOwnRequest, unregisterRequest)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global   (State)


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
    whenM (isOwnRequest uuid) $ unregisterRequest uuid

doNothing :: a -> Command State ()
doNothing = const $ return ()
