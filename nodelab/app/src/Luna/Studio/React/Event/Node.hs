{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.React.Event.Node where

import           Data.Aeson                  (FromJSON, ToJSON)
import           Empire.API.Data.NodeLoc     (NodeLoc)
import           Empire.API.Data.PortDefault (PortDefault)
import           Luna.Studio.Data.PortRef    (AnyPortRef)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Action    (InitValue)
import           React.Flux                  (KeyboardEvent, MouseEvent)


data Event = DisplayResultChanged Bool          NodeLoc
           | EditExpression                     NodeLoc
           | Enter                              NodeLoc
           | MouseDown            MouseEvent    NodeLoc
           | NameEditApply                      NodeLoc Text
           | NameEditDiscard                    NodeLoc
           | NameEditStart                      NodeLoc
           | PortApplyString      KeyboardEvent AnyPortRef PortDefault
           | PortEditString                     AnyPortRef PortDefault
           | PortInitSlider          MouseEvent AnyPortRef InitValue
           | PortSetPortDefault                 AnyPortRef PortDefault
           | Select               MouseEvent    NodeLoc
           | SetCode                            NodeLoc Text
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
