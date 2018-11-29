{-# LANGUAGE Rank2Types #-}
module NodeEditor.Event.Preprocessor.Batch (process) where

import           Common.Prelude                    hiding (cons)
import           Data.Binary                       (Binary, decode)
import           Data.ByteString.Lazy.Char8        (ByteString)
import qualified Data.Map.Lazy                     as Map
import qualified GZip

import           Common.Batch.Connector.Connection (ControlCode (ConnectionTakeover, Welcome), WebMessage (ControlMessage, WebMessage))
import qualified LunaStudio.API.Topic              as Topic
import           NodeEditor.Event.Batch            as Batch
import           NodeEditor.Event.Connection       as Connection
import qualified NodeEditor.Event.Event            as Event

process :: Event.Event -> Maybe Event.Event
process (Event.Connection (Message msg)) = Just $ Event.Batch $ processMessage msg
process _                                = Nothing

handle :: forall a. (Binary a, Topic.MessageTopic a) => (a -> Batch.Event) -> (String, ByteString -> Batch.Event)
handle cons = (Topic.topic @a, cons . decode . GZip.decompress)

handlers :: Map.Map String (ByteString -> Batch.Event)
handlers = Map.fromList [ handle AddConnectionResponse
                        , handle AddImportsResponse
                        , handle AddNodeResponse
                        , handle AddPortResponse
                        , handle AddSubgraphResponse
                        , handle AtomPasteResponse
                        , handle AutolayoutNodesResponse
                        , handle CollaborationUpdate
                        , handle CollapseToFunctionResponse
                        , handle CopyResponse
                        , handle DumpGraphVizResponse
                        , handle EmpireStarted
                        , handle GetProgramResponse
                        , handle GetSubgraphsResponse
                        , handle MonadsUpdate
                        , handle MovePortResponse
                        , handle NodeResultUpdate
                        , handle NodeTypecheckerUpdate
                        , handle PasteResponse
                        , handle ProjectMoved
                        , handle RedoResponse
                        , handle RemoveConnectionResponse
                        , handle RemoveNodesResponse
                        , handle RemovePortResponse
                        , handle RenameNodeResponse
                        , handle RenamePortResponse
                        , handle SearchNodesResponse
                        , handle SetCodeResponse
                        , handle SetNodeExpressionResponse
                        , handle SetNodesMetaResponse
                        , handle SetPortDefaultResponse
                        , handle SubstituteResponse
                        , handle TransactionResponse
                        , handle TypeCheckResponse
                        , handle UndoResponse
                        ]

processMessage :: WebMessage -> Batch.Event
processMessage (WebMessage topic bytes) = handler bytes where
    handler      = Map.findWithDefault defHandler topic handlers
    defHandler _ = UnknownEvent topic
processMessage (ControlMessage ConnectionTakeover) = ConnectionDropped
processMessage (ControlMessage Welcome)            = ConnectionOpened
