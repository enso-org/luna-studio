{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module WSConnector.Data.WSFrame where

import qualified Data.Binary                 as Binary
import           Data.ByteString             (ByteString)
import           Data.ByteString.Base64.Lazy (decodeLenient, encode)
import           Data.ByteString.Lazy        (fromStrict, toStrict)
import           Prologue

import           WSConnector.Data.WSMessage

newtype WSFrame = WSFrame { _messages :: [WSMessage]
                          } deriving (Show, Generic)

makeLenses ''WSFrame
instance Binary.Binary WSFrame

deserializeFrame :: ByteString -> WSFrame
deserializeFrame = Binary.decode . decodeLenient . fromStrict

serializeFrame :: WSFrame -> ByteString
serializeFrame = toStrict . encode . Binary.encode

