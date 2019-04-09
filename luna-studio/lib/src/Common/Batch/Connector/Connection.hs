{-# LANGUAGE DeriveAnyClass #-}
module Common.Batch.Connector.Connection where

import           Common.Prelude              hiding (Text)
import           Data.Binary                 (Binary)
import qualified Data.Binary                 as Binary
import qualified Data.ByteString.Lazy        as ByteString
import qualified Data.ByteString             as Strict
import           Data.ByteString.Lazy.Char8  (ByteString, pack)
import           Data.JSString.Text
import           Data.Text.Lazy.Encoding     (decodeUtf8)
import           Data.UUID.Types             (UUID)
import qualified GZip
import           LunaStudio.API.Request      (Request (..))
import qualified LunaStudio.API.Topic        as Topic
import           WebSocket

data ControlCode = ConnectionTakeover
                 | Welcome
                 deriving (Eq, Generic, NFData, Show)

data WebMessage = WebMessage { _topic   :: String
                             , _message :: ByteString
                             }
                | ControlMessage ControlCode
                deriving (Eq, Generic, NFData, Show)

data Message a = Message { _reqUUID :: UUID
                         , _guiID   :: Maybe UUID
                         , _request :: a }
                         deriving (Generic, Show)

data Frame = Frame
    { _messages :: [WebMessage]
    } deriving (Show, Generic)

makeLenses ''Frame
makeLenses ''Message
makeLenses ''WebMessage

type BinaryRequest a = (Topic.MessageTopic (Request a), Binary a)
type BinaryMessage a = (Topic.MessageTopic a, Binary a)

instance (Binary a) => Binary (Message a)
instance Binary.Binary ControlCode
instance Binary.Binary WebMessage
instance Binary.Binary Frame

serialize :: Frame -> Strict.ByteString
serialize = ByteString.toStrict . Binary.encode

deserialize :: Strict.ByteString -> Frame
deserialize = Binary.decode . ByteString.fromStrict

sendMessages :: MonadIO m => [WebMessage] -> m ()
sendMessages msgs = liftIO $ do
    socket <- getWebSocket
    send socket . serialize $ Frame msgs

sendMessage :: MonadIO m => WebMessage -> m ()
sendMessage msg = sendMessages [msg]

makeMessage :: BinaryRequest a => Message a -> WebMessage
makeMessage msg =
    let body = Request (msg ^. reqUUID) (msg ^. guiID) (msg ^. request)
    in makeMessage' body

makeMessage' :: BinaryMessage a => a -> WebMessage
makeMessage' body =
    WebMessage (Topic.topic' body) (GZip.compress $ Binary.encode body)

sendRequest :: (MonadIO m, BinaryRequest a) => Message a -> m ()
sendRequest = sendMessage . makeMessage

sendUpdate :: (MonadIO m, BinaryMessage a) => a -> m ()
sendUpdate = sendMessage . makeMessage'

sendRequests :: (MonadIO m, BinaryRequest a) => [Message a] -> m ()
sendRequests msgs = sendMessages $ makeMessage <$> msgs
