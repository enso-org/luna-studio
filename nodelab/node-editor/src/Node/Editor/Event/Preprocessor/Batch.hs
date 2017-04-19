{-# LANGUAGE Rank2Types #-}
module Node.Editor.Event.Preprocessor.Batch (process) where

import           Data.Binary                            (Binary, decode)
import           Data.ByteString.Lazy.Char8             (ByteString)
import qualified Data.Map.Lazy                          as Map
import           Luna.Prelude                    hiding (cons)

import qualified Empire.API.Topic                       as Topic
import           Luna.Batch.Connector.Connection (ControlCode (ConnectionTakeover, Welcome), WebMessage (ControlMessage, WebMessage))
import           Node.Editor.Event.Batch                as Batch
import           Node.Editor.Event.Connection           as Connection
import qualified Node.Editor.Event.Event                as Event


process :: Event.Event -> Maybe Event.Event
process (Event.Connection (Message msg)) = Just $ Event.Batch $ processMessage msg
process _                                = Nothing

handle :: forall a. (Binary a, Topic.MessageTopic a) => (a -> Batch.Event) -> (String, ByteString -> Batch.Event)
handle cons = (Topic.topic (undefined :: a), cons . decode)

handlers :: Map.Map String (ByteString -> Batch.Event)
handlers = Map.fromList [ handle GetProgramResponse
                        , handle AddConnectionResponse
                        , handle AddNodeResponse
                        , handle AddPortResponse
                        , handle AddSubgraphResponse
                        , handle CodeUpdate
                        , handle CollaborationUpdate
                        , handle ConnectUpdate
                        , handle DumpGraphVizResponse
                        , handle EmpireStarted
                        , handle GetSubgraphsResponse
                        , handle MonadsUpdate
                        , handle MovePortResponse
                        , handle NodeResultUpdate
                        , handle NodesUpdate
                        , handle NodeTypecheckerUpdate
                        , handle RedoResponse
                        , handle RemoveConnectionResponse
                        , handle RemoveConnectionUpdate
                        , handle RemoveNodesResponse
                        , handle RemovePortResponse
                        , handle RenameNodeResponse
                        , handle RenamePortResponse
                        , handle SearchNodesResponse
                        , handle SetNodeCodeResponse
                        , handle SetNodeExpressionResponse
                        , handle SetNodesMetaResponse
                        , handle SetPortDefaultResponse
                        , handle TypeCheckResponse
                        , handle UndoResponse
                        , handle ProjectCreated
                        , handle ProjectCreatedUpdate
                        , handle ProjectExported
                        , handle ProjectImported
                        , handle ProjectList
                        , handle ProjectOpened
                        , handle ProjectOpenedUpdate
                        ]

processMessage :: WebMessage -> Batch.Event
processMessage (WebMessage topic bytes) = handler bytes where
    handler      = Map.findWithDefault defHandler topic handlers
    defHandler _ = UnknownEvent topic
processMessage (ControlMessage ConnectionTakeover) = ConnectionDropped
processMessage (ControlMessage Welcome)            = ConnectionOpened
