{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Luna.Studio.React.Event.Edge where

import           Data.Aeson              (FromJSON, ToJSON)
import           Empire.API.Data.Node    (NodeId)
import           Empire.API.Data.PortRef (AnyPortRef)
import           Luna.Studio.Prelude
import           React.Flux              (MouseEvent)



data Event = AddPort       NodeId
           | MouseMove     MouseEvent NodeId
           | RemovePort
           | PortNameApply     AnyPortRef Text
           | PortNameDiscard   AnyPortRef
           | PortNameStartEdit AnyPortRef
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
