{-# LANGUAGE DeriveAnyClass #-}

module Luna.Studio.React.Event.Connection where

import           Control.DeepSeq            (NFData)
import           Data.Aeson                 (FromJSON, ToJSON)
import           React.Flux                 (MouseEvent)

import           Empire.API.Data.Connection (ConnectionId)
import           Empire.API.Data.PortRef    (AnyPortRef)
import           Luna.Studio.Prelude



data ModifiedEnd = Source | Destination deriving (Eq, Generic, NFData, Show, Typeable)

instance ToJSON   ModifiedEnd
instance FromJSON ModifiedEnd

data Event = StartConnection  MouseEvent AnyPortRef
           | EndConnection    MouseEvent AnyPortRef
           | ModifyConnection MouseEvent ConnectionId ModifiedEnd
           | Click            MouseEvent AnyPortRef
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
