{-# LANGUAGE Rank2Types #-}
module Luna.Studio.Event.Preprocessor.Batch (process) where

import           Data.Binary                            (Binary, decode)
import           Data.ByteString.Lazy.Char8             (ByteString)
import qualified Data.Map.Lazy                          as Map
import           Luna.Studio.Prelude                    hiding (cons)

import qualified Empire.API.Topic                       as Topic
import           Luna.Studio.Batch.Connector.Connection (ControlCode (ConnectionTakeover, Welcome), WebMessage (ControlMessage, WebMessage))
import           Luna.Studio.Event.Batch                as Batch
import           Luna.Studio.Event.Connection           as Connection
import qualified Luna.Studio.Event.Event                as Event


process :: Event.Event -> Maybe Event.Event
process (Event.Connection (Message msg)) = Just $ Event.Batch $ processMessage msg
process _                                = Nothing

handle :: forall a. (Binary a, Topic.MessageTopic a) => (a -> Batch.Event) -> (String, ByteString -> Batch.Event)
handle cons = (Topic.topic (undefined :: a), cons . decode)

handlers :: Map.Map String (ByteString -> Batch.Event)
handlers = Map.fromList [ handle GetProgramResponse
                        , handle AddNodeResponse
                        , handle AddPortResponse
                        , handle AddSubgraphResponse
                        , handle CodeUpdate
                        , handle CollaborationUpdate
                        , handle ConnectResponse
                        , handle ConnectUpdate
                        , handle DumpGraphVizResponse
                        , handle EmpireStarted
                        , handle MonadsUpdate
                        , handle MovePortResponse

                        , handle RemoveConnectionResponse
                        , handle RemoveConnectionUpdate
                        , handle NodeMetaResponse
                        , handle NodeMetaUpdated
                        , handle NodeRenamed
                        , handle NodeResultUpdated
                        , handle NodeSearchResponse
                        , handle NodesRemoved
                        , handle NodesUpdated
                        , handle NodeTypechecked
                        , handle PortRenamed
                        , handle ProjectCreated
                        , handle ProjectCreatedUpdate
                        , handle ProjectExported
                        , handle ProjectImported
                        , handle ProjectList
                        , handle ProjectOpened
                        , handle ProjectOpenedUpdate
                        , handle RemoveNodesResponse
                        , handle RemovePortResponse
                        , handle UpdateNodeExpressionResponse
                        ]

processMessage :: WebMessage -> Batch.Event
processMessage (WebMessage topic bytes) = handler bytes where
    handler      = Map.findWithDefault defHandler topic handlers
    defHandler _ = UnknownEvent topic
processMessage (ControlMessage ConnectionTakeover) = ConnectionDropped
processMessage (ControlMessage Welcome)            = ConnectionOpened
