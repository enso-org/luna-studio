{-# LANGUAGE DeriveAnyClass #-}

module Luna.Studio.React.Event.Node where

import           Data.Aeson                   (FromJSON, ToJSON)
import           React.Flux                   (KeyboardEvent, MouseEvent)

import           Empire.API.Data.DefaultValue (PortDefault)
import           Empire.API.Data.Node         (NodeId)
import           Empire.API.Data.PortRef      (AnyPortRef)
import           Luna.Studio.Prelude
import qualified Luna.Studio.State.Action     as Action



data Event = DisplayResultChanged Bool          NodeId
           | EditExpression                     NodeId
           | Enter                              NodeId
           | MouseDown            MouseEvent    NodeId
           | NameApply                          NodeId
           | NameChange           Text          NodeId
           | NameDiscard                        NodeId
           | NameEditStart                      NodeId
           | PortApplyString      KeyboardEvent AnyPortRef PortDefault
           | PortEditString                     AnyPortRef PortDefault
           | PortInitSlider          MouseEvent AnyPortRef Action.InitValue
           | PortSetDefaultValue                AnyPortRef PortDefault
           | Select               MouseEvent    NodeId
           | SetCode                            NodeId Text
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
