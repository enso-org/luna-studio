{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.Batch.Connector.Connection where

import           Data.Binary                 (Binary)
import qualified Data.Binary                 as Binary
import qualified Data.ByteString.Base64.Lazy as Base64
import           Data.ByteString.Lazy.Char8  (ByteString, pack)
import           Data.JSString.Text
import           Data.Text.Lazy.Encoding     (decodeUtf8)
import           Data.UUID.Types             (UUID)
import           Empire.API.Request          (Request (..))
import qualified Empire.API.Topic            as Topic
import           JS.WebSocket
import           Luna.Studio.Prelude         hiding (Text)

data ControlCode = ConnectionTakeover
                 | Welcome
                 deriving (Eq, Generic, NFData, Show)

instance Binary.Binary ControlCode

data WebMessage = WebMessage { _topic   :: String
                             , _message :: ByteString
                             }
                | ControlMessage ControlCode
                deriving (Eq, Generic, NFData, Show)

makeLenses ''WebMessage
instance Binary.Binary WebMessage

data Message a = Message { _reqUUID :: UUID
                         , _guiID   :: Maybe UUID
                         , _request :: a }
                         deriving (Generic, Show)

instance (Binary a) => Binary (Message a)

data Frame = Frame { _messages :: [WebMessage] } deriving (Show, Generic)

makeLenses ''Frame
instance Binary.Binary Frame

serialize :: Frame -> JSString
serialize = lazyTextToJSString . decodeUtf8 . Base64.encode . Binary.encode

deserialize :: String -> Frame
deserialize = Binary.decode . Base64.decodeLenient . pack

sendMessages :: [WebMessage] -> IO ()
sendMessages msgs = do
    putStrLn "before getWebSocket"
    socket <- getWebSocket
    putStrLn $ show msgs
    putStrLn "sendMessages 1"
    let serialized = serialize $ Frame msgs
    putStrLn "sendMessages 2"
    send socket serialized
    putStrLn "sendMessages 3"

sendMessage :: WebMessage -> IO ()
sendMessage msg = do
  putStrLn "sendMessage 1"
  sendMessages [msg]
  putStrLn "sendMessage 2"

makeMessage :: (Topic.MessageTopic (Request a), Binary a) => Message a -> WebMessage
makeMessage (Message uuid guiID body) = let body' = Request uuid guiID body in WebMessage (Topic.topic body') (Binary.encode body')

makeMessage' :: (Topic.MessageTopic a, Binary a) => a -> WebMessage
makeMessage' body = let body' = body in WebMessage (Topic.topic body') (Binary.encode body')

sendRequest :: (Topic.MessageTopic (Request a), Binary a) => Message a -> IO ()
sendRequest = sendMessage . makeMessage

sendUpdate :: (Topic.MessageTopic a, Binary a) => a -> IO ()
sendUpdate = sendMessage . makeMessage'

sendRequests :: (Topic.MessageTopic (Request a), Binary a) => [Message a] -> IO ()
sendRequests msgs = sendMessages $ makeMessage <$> msgs
