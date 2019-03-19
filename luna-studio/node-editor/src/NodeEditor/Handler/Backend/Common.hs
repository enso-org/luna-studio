module NodeEditor.Handler.Backend.Common where

import Common.Prelude

import qualified Data.Text                    as Text
import qualified LunaStudio.API.Graph.Request as Request
import qualified LunaStudio.API.Response      as Response
import qualified LunaStudio.API.Topic         as Topic
import qualified LunaStudio.Data.Error        as ErrorAPI

import Common.Action.Command               (Command)
import Common.Batch.Connector.Connection   (BinaryRequest, Message (Message),
                                            sendRequest)
import Common.Report                       (error)
import Luna.Benchmark                      (measureResponseTime)
import LunaStudio.API.Graph.Request        (GraphRequest)
import LunaStudio.API.Request              (Request)
import LunaStudio.Data.Error               (Error, LunaError)
import NodeEditor.Action.Batch             (withWorkspace)
import NodeEditor.Action.UUID              (isOwnRequest, unregisterRequest)
import NodeEditor.Batch.Connector.Commands (withLibrary)
import NodeEditor.State.Global             (State)


whenOk :: Response.Response req inv res -> (res -> Command State ()) -> Command State ()
whenOk (Response.Response _ _ _ _ (Response.Ok    res)) handler = handler res
whenOk (Response.Response _ _ _ _ (Response.Error _  )) _       = return ()

handleResponse :: (Topic.MessageTopic (Response.Response req inv res), Show req)
    => Response.Response req inv res
    -> (res -> Command State ())
    -> (ErrorAPI.Error ErrorAPI.LunaError -> Response.Status inv -> Command State ())
    -> Command State ()
handleResponse resp@(Response.Response uuid _ req inv res) success failure = do
    case res of
        Response.Ok    res' -> success res'
        Response.Error err  -> failure err inv
    measureResponseTime resp
    whenM (isOwnRequest uuid) $ unregisterRequest uuid

doNothing2 :: a -> b -> Command State ()
doNothing2 _ _ = return ()

doNothing :: a -> Command State ()
doNothing = const $ return ()

logError :: Topic.MessageTopic (Response.Response req inv res)
    => Response.Response req inv res -> Error LunaError -> Command State ()
logError response err = error errStr where
    str = Text.unpack $ err ^. ErrorAPI.errorContent
    errStr = str <> "\n\nwhile processing event\n\n" <> Topic.topic' response

resend :: (BinaryRequest r, GraphRequest r, Topic.MessageTopic (Request r))
    => r -> Command State ()
resend r = withWorkspace resendRequest where
    resendRequest workspace reqId guiId = sendRequest $ Message
        reqId
        guiId
        . withLibrary workspace $ \gl -> r & Request.location .~ gl
