module Luna.Studio.Handler.Backend.Common
    ( whenOk
    , handleResponse
    , doNothing
    ) where

import qualified Data.Aeson                 as JSON (ToJSON)
import qualified Data.UUID.Types            as UUID (toString)
import           Luna.Studio.Prelude

import           Luna.Studio.Action.Command (Command)
import           Luna.Studio.Action.UUID    (isOwnRequest, unregisterRequest)
import           Luna.Studio.State.Global   (State)

import qualified Empire.API.Response        as Response
import qualified Empire.API.Topic           as Topic
import qualified JS.Debug                   as Debug

whenOk :: Response.Response req res -> (res -> Command State ()) -> Command State ()
whenOk (Response.Response _ _ (Response.Ok    res)) handler = handler res
whenOk (Response.Response _ _ (Response.Error _  )) _       = return ()

handleResponse :: (Topic.MessageTopic (Response.Response req res), JSON.ToJSON req) => Response.Response req res -> (req -> res -> Command State ()) -> Command State ()
handleResponse resp@(Response.Response uuid req status) success =
    whenM (isOwnRequest uuid) $ do
        unregisterRequest uuid
        case status of
            Response.Ok result -> success req result
            Response.Error str -> liftIO $ Debug.error (convert $ Topic.topic resp <> " [" <> UUID.toString uuid <> "] " <> str) req

doNothing :: a -> b -> Command State ()
doNothing _ _ = return ()
