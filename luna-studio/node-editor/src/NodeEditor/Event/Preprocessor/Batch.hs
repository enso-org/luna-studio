{-# LANGUAGE Rank2Types #-}
module NodeEditor.Event.Preprocessor.Batch (process) where

import qualified Codec.Compression.GZip            as GZip
import           Common.Prelude                    hiding (cons)
import           Data.Binary                       (Binary, decode)
import           Data.ByteString.Lazy.Char8        (ByteString)
import qualified Data.ByteString.Lazy              as BSL
import qualified Data.Map.Lazy                     as Map

import           Common.Batch.Connector.Connection (ControlCode (ConnectionTakeover, Welcome), WebMessage (ControlMessage, WebMessage))
import qualified LunaStudio.API.Topic              as Topic
import           NodeEditor.Event.Batch            as Batch
import           NodeEditor.Event.Connection       as Connection
import qualified NodeEditor.Event.Event            as Event

import System.IO.Unsafe (unsafePerformIO)

process :: Event.Event -> Maybe Event.Event
process (Event.Connection (Message msg)) = Just $ Event.Batch $ processMessage msg
process _                                = Nothing

decompressWithDebug :: BSL.ByteString -> BSL.ByteString
decompressWithDebug d = unsafePerformIO $ do
    putStrLn "===== Decompressing data ====="
    print d
    putStrLn "===== o takie ================"
    return $ GZip.decompress d

handle :: forall a. (Binary a, Topic.MessageTopic a) => (a -> Batch.Event) -> (String, ByteString -> Batch.Event)
handle cons = (Topic.topic (undefined :: a), cons . decode . decompressWithDebug)

handlers :: Map.Map String (ByteString -> Batch.Event)
handlers = Map.fromList [ handle AddConnectionResponse
                        , handle AddNodeResponse
                        , handle AddPortResponse
                        , handle AddSubgraphResponse
                        , handle AutolayoutNodesResponse
                        , handle CollaborationUpdate
                        , handle DumpGraphVizResponse
                        , handle EmpireStarted
                        , handle GetProgramResponse
                        , handle GetSubgraphsResponse
                        , handle MonadsUpdate
                        , handle MovePortResponse
                        , handle NodeResultUpdate
                        , handle NodeTypecheckerUpdate
                        , handle ProjectCreated
                        , handle ProjectCreatedUpdate
                        , handle ProjectExported
                        , handle ProjectImported
                        , handle ProjectList
                        , handle ProjectOpened
                        , handle ProjectOpenedUpdate
                        , handle RedoResponse
                        , handle RemoveConnectionResponse
                        , handle RemoveNodesResponse
                        , handle RemovePortResponse
                        , handle RenameNodeResponse
                        , handle RenamePortResponse
                        , handle SearchNodesResponse
                        , handle SetNodeExpressionResponse
                        , handle SetNodesMetaResponse
                        , handle SetPortDefaultResponse
                        , handle SubstituteResponse
                        , handle TypeCheckResponse
                        , handle UndoResponse
                        ]

processMessage :: WebMessage -> Batch.Event
processMessage (WebMessage topic bytes) = handler bytes where
    handler      = Map.findWithDefault defHandler topic handlers
    defHandler _ = UnknownEvent topic
processMessage (ControlMessage ConnectionTakeover) = ConnectionDropped
processMessage (ControlMessage Welcome)            = ConnectionOpened
