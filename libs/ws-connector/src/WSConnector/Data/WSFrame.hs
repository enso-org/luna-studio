{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module WSConnector.Data.WSFrame where

import qualified Data.Binary                 as Binary
import           Data.ByteString             (ByteString)
import           Data.ByteString.Lazy        (fromStrict, toStrict)
import           Prologue

import           WSConnector.Data.WSMessage

newtype WSFrame = WSFrame { _messages :: [WSMessage]
                          } deriving (Show, Generic)

makeLenses ''WSFrame
instance Binary.Binary WSFrame

deserializeFrame :: ByteString -> WSFrame
deserializeFrame = Binary.decode . fromStrict

serializeFrame :: WSFrame -> ByteString
serializeFrame = toStrict . Binary.encode

