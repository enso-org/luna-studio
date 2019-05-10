module Bus.Data.Message where

import Prologue

import qualified Data.ByteString as ByteString

import qualified Data.Binary.Put as Put
import qualified Data.Binary.Get as Get
import qualified Data.Text.Encoding as Encoding
import qualified Data.Text as Text

import Data.Binary (Put, Get)
import Data.ByteString (ByteString)


-- === Definition === --

type Topic = String

-- | We do not attach any additional meaning to the parts of the message,
--   in this library a message is just an arbitrary String, with the body
--   being an arbitrary ByteString.
--   The topic is used to identify particular message and an of its prefixes
--   can be used by clients to subscribe on particular messages.
--   It is advised that clients of this library build some more typesafe
--   structure on top of this.
data Message = Message
    { _topic :: Topic
    , _body  :: ByteString
    }

makeLenses ''Message


-- === Serialization === --

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


-- === Utils === --

getUntilSeparator :: Word8 -> Get ByteString
getUntilSeparator sep = ByteString.pack <$> go where
    go = do
        item <- Get.getWord8
        if item == sep then pure [] else (item :) <$> go
