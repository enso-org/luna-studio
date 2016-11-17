module Undo.Undo where


import qualified ZMQ.Bus.Bus                       as Bus
import qualified ZMQ.Bus.Data.Message              as Message
import           ZMQ.Bus.Data.MessageFrame         (MessageFrame (MessageFrame))
import           ZMQ.Bus.Data.Topic                (Topic)
import           ZMQ.Bus.EndPoint                  (BusEndPoints)
import           ZMQ.Bus.Trans                     (BusT (..))
import qualified ZMQ.Bus.Trans                     as Bus



data UndoItem =  UndoItem { _event :: Maybe Request } deriving (Show, Eq, Generic) --cos innego tutaj niz request ale jeszcze ie weim co

newtype UndoList = UndoList { _items :: [UndoItem] } deriving (Show, Eq, Generic)



collect :: BusEndPoints -> [Topic] -> UndoList
collect endPoints topics = Bus.runBus endPoints $ do
    mapM_ Bus.subscribe topics
    Bus.runBusT $ evalStateT $ forever collectEvents def

collectEvents :: UndoList
collectEvents = do
    msgFrame <- lift $ BusT Bus.receive'
    case msgFrame of
        Left err -> Nothing
        Right (MessageFrame msg corId senderId lastFrm) -> do
            let topic = msg ^. Message.topic
                userId = show senderId
                content = msg ^. Message.message
            case Utils.lastPart '.' topic of
                "request"  -> collectedMessage userId topic content
                "response" -> collectedMessage userId topic content
                "update"   -> collectedMessage userId topic content

collectedMessage :: String -> String -> ByteString -> BusT ()
collectedMessage userId topic content = do
