module TextEditor.Handler.Backend.Common
    ( whenOk
    , handleResponse
    , doNothing
    , doNothing2
    ) where

import Common.Prelude

import Common.Action.Command   (Command)
import Common.Report           (error)
import Luna.Benchmark          (measureResponseTime)
import TextEditor.Action.UUID  (isOwnRequest, unregisterRequest)
import TextEditor.State.Global (State)

import qualified Data.Text               as Text
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as Topic
import qualified LunaStudio.Data.Error   as ErrorAPI


whenOk :: Response.Response req inv res -> (res -> Command State ()) -> Command State ()
whenOk (Response.Response _ _ _ _ (Response.Ok    res)) handler = handler res
whenOk (Response.Response _ _ _ _ (Response.Error _  )) _       = return ()

handleResponse :: (Topic.MessageTopic (Response.Response req inv res), Show req) => Response.Response req inv res -> (res -> Command State ()) -> (ErrorAPI.Error ErrorAPI.LunaError -> Response.Status inv -> Command State ()) -> Command State ()
handleResponse resp@(Response.Response uuid _ _ inv res) success failure = do
    case res of
        Response.Ok    res' -> success res'
        Response.Error err  -> do
            let str = Text.unpack $ err ^. ErrorAPI.errorContent
            error $ str <> "\n\nwhile processing event\n\n" <> Topic.topic' resp
            failure err inv
    measureResponseTime resp
    whenM (isOwnRequest uuid) $ unregisterRequest uuid

doNothing2 :: a -> b -> Command State ()
doNothing2 _ _ = return ()

doNothing :: a -> Command State ()
doNothing = const $ return ()
