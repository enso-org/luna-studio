{-# LANGUAGE DeriveAnyClass #-}

module Luna.Studio.React.Event.Connection where

import           Control.DeepSeq            (NFData)
import           Data.Aeson                 (FromJSON, ToJSON)
import           React.Flux                 (MouseEvent)

import           Empire.API.Data.Connection (ConnectionId)
import           Empire.API.Data.PortRef    (AnyPortRef)
import           Empire.API.JSONInstances   ()
import           Luna.Studio.Prelude

data Event = StartConnection  MouseEvent AnyPortRef
           | EndConnection    MouseEvent AnyPortRef
           | ModifyConnection MouseEvent ConnectionId
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
