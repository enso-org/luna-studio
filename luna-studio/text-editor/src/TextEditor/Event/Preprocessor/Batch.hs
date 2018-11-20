{-# LANGUAGE Rank2Types #-}
module TextEditor.Event.Preprocessor.Batch (process) where

import           Common.Prelude                    hiding (cons)
import           Data.Binary                       (Binary, decode)
import           Data.ByteString.Lazy.Char8        (ByteString)
import           Data.Map.Lazy                     (Map)
import qualified Data.Map.Lazy                     as Map
import qualified GZip

import           Common.Batch.Connector.Connection (ControlCode (ConnectionTakeover, Welcome), WebMessage (ControlMessage, WebMessage))
import qualified LunaStudio.API.Topic              as Topic
import           TextEditor.Event.Batch            (BatchEvent (..))
import           TextEditor.Event.Connection       as Connection
import qualified TextEditor.Event.Event            as Event


process :: Event.Event -> Maybe Event.Event
process (Event.Connection (Message msg)) = Just $ Event.Batch $ processMessage msg
process _                                = Nothing

handle :: forall a. (Binary a, Topic.MessageTopic a) => (a -> BatchEvent) -> (String, ByteString -> BatchEvent)
handle cons = (Topic.topic @a, cons . decode . GZip.decompress)

handlers :: Map String (ByteString -> BatchEvent)
handlers = Map.fromList [ handle BufferGetResponse
                        , handle CopyResponse
                        , handle EmpireStarted
                        , handle FileClosed
                        , handle FileOpened
                        , handle FileSaved
                        , handle InterpreterResponse
                        , handle InterpreterUpdate
                        , handle ProjectCreated
                        , handle ProjectMove
                        , handle ProjectSet
                        , handle SubstituteResponse
                        , handle SubstituteUpdate
                        ]

processMessage :: WebMessage -> BatchEvent
processMessage (WebMessage topic bytes) = handler bytes where
    handler      = Map.findWithDefault defHandler topic handlers
    defHandler _ = UnknownEvent topic
processMessage (ControlMessage ConnectionTakeover) = ConnectionDropped
processMessage (ControlMessage Welcome)            = ConnectionOpened
