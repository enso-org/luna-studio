{-# LANGUAGE DeriveAnyClass #-}
module Common.Batch.Connector.Connection where

import           Data.Binary                 (Binary)
import qualified Data.Binary                 as Binary
import qualified Data.ByteString.Base64.Lazy as Base64
import           Data.ByteString.Lazy.Char8  (ByteString, pack)
import           Data.JSString.Text
import           Data.Text.Lazy.Encoding     (decodeUtf8)
import           Data.UUID.Types             (UUID)
import           LunaStudio.API.Request          (Request (..))
import qualified LunaStudio.API.Topic            as Topic
import           WebSocket
import           Common.Prelude         hiding (Text)

import System.IO.Unsafe (unsafePerformIO)
import qualified GZip

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

data Frame = Frame { _messages :: [WebMessage] } deriving (Show, Generic)

makeLenses ''Frame
makeLenses ''WebMessage

type BinaryRequest a = (Topic.MessageTopic (Request a), Binary a)
type BinaryMessage a = (Topic.MessageTopic a, Binary a)

instance (Binary a) => Binary (Message a)
instance Binary.Binary ControlCode
instance Binary.Binary WebMessage
instance Binary.Binary Frame

serialize :: Frame -> JSString
serialize = lazyTextToJSString . decodeUtf8 . Base64.encode . Binary.encode

deserialize :: String -> Frame
deserialize = Binary.decode . Base64.decodeLenient . pack

sendMessages :: [WebMessage] -> IO ()
sendMessages msgs = do
    socket <- getWebSocket
    send socket $ serialize $ Frame msgs

sendMessage :: WebMessage -> IO ()
sendMessage msg = sendMessages [msg]

makeMessage :: BinaryRequest a => Message a -> WebMessage
makeMessage (Message uuid guiID body) = let body' = Request uuid guiID body in WebMessage (Topic.topic body') (GZip.compress $ Binary.encode body')

makeMessage' :: BinaryMessage a => a -> WebMessage
makeMessage' body = let body' = body in WebMessage (Topic.topic body') (GZip.compress $ Binary.encode body')

sendRequest :: BinaryRequest a => Message a -> IO ()
sendRequest m = sendMessage $ makeMessage m

sendUpdate :: BinaryMessage a => a -> IO ()
sendUpdate m = sendMessage $ makeMessage' m

sendRequests :: BinaryRequest a => [Message a] -> IO ()
sendRequests msgs = sendMessages $ makeMessage <$> msgs
