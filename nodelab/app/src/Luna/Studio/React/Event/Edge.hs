{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Luna.Studio.React.Event.Edge where

import           Data.Aeson              (FromJSON, ToJSON)
import           Empire.API.Data.Node    (NodeId)
import           Empire.API.Data.PortRef (AnyPortRef)
import           Luna.Studio.Prelude
import           React.Flux              (MouseEvent)



data Event = AddPort           AnyPortRef
           | MouseMove         MouseEvent NodeId
           | RemovePort        AnyPortRef
           | PortNameApply     AnyPortRef String
           | PortNameDiscard   AnyPortRef
           | PortNameStartEdit AnyPortRef
           | ToggleEdgeMode    NodeId
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
