module Bus.Data.Message where

import Prologue

import qualified Data.ByteString as ByteString

import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text as Text

import Data.Binary (Put, Get)
import Data.ByteString (ByteString)

type Topic = String

data Message = Message
    { _topic :: Topic
    , _body  :: ByteString
    }

makeLenses ''Message

encodeTopic :: Topic -> ByteString
encodeTopic = Encoding.encodeUtf8 . convert

decodeTopic :: ByteString -> Topic
decodeTopic = convert . Encoding.decodeUtf8

encode :: Message -> ByteString
encode (Message topic body) = convert . Put.runPut $ do
    Put.putByteString $ encodeTopic topic
    Put.putWord8 0
    Put.putByteString body

decode :: ByteString -> Message
decode bytes = flip Get.runGet (convert bytes) $ do
    topic <- getUntilSeparator 0
    body  <- convert <$> Get.getRemainingLazyByteString
    pure $ Message (decodeTopic topic) body

getUntilSeparator :: Word8 -> Get ByteString
getUntilSeparator sep = ByteString.pack <$> go where
    go = do
        item <- Get.getWord8
        if item == sep then pure [] else (item :) <$> go
