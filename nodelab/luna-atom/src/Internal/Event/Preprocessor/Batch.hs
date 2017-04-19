{-# LANGUAGE Rank2Types #-}
module Internal.Event.Preprocessor.Batch (process) where

import           Data.Binary                            (Binary, decode)
import           Data.ByteString.Lazy.Char8             (ByteString)
import qualified Data.Map.Lazy                          as Map
import           Luna.Prelude                    hiding (cons)

import qualified Empire.API.Topic                       as Topic
import           Luna.Batch.Connector.Connection (ControlCode (ConnectionTakeover, Welcome), WebMessage (ControlMessage, WebMessage))
import           Internal.Event.Batch                as Batch
import           Internal.Event.Connection           as Connection
import qualified Internal.Event.Event                as Event


process :: Event.Event -> Maybe Event.Event
process (Event.Connection (Message msg)) = Just $ Event.Batch $ processMessage msg
process _                                = Nothing

handle :: forall a. (Binary a, Topic.MessageTopic a) => (a -> Batch.Event) -> (String, ByteString -> Batch.Event)
handle cons = (Topic.topic (undefined :: a), cons . decode)

handlers :: Map.Map String (ByteString -> Batch.Event)
handlers = Map.fromList [ handle EmpireStarted
                        , handle ProjectSet
                        , handle FileClosed
                        , handle FileOpened
                        , handle FileSaved
                        , handle BufferGetResponse
                        , handle SubstituteResponse
                        , handle SubstituteUpdate
                        ]

processMessage :: WebMessage -> Batch.Event
processMessage (WebMessage topic bytes) = handler bytes where
    handler      = Map.findWithDefault defHandler topic handlers
    defHandler _ = UnknownEvent topic
processMessage (ControlMessage ConnectionTakeover) = ConnectionDropped
processMessage (ControlMessage Welcome)            = ConnectionOpened
