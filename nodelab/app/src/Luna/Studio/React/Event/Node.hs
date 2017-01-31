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
           | Select               MouseEvent    NodeId
           | NameEditStart                      NodeId
           | NameKeyDown          KeyboardEvent NodeId
           | NameChange           Text          NodeId
           | PortSetDefaultValue                AnyPortRef PortDefault
           | PortApplyString      KeyboardEvent AnyPortRef PortDefault
           | PortEditString                     AnyPortRef PortDefault
           | PortInitSlider          MouseEvent AnyPortRef Action.InitValue
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
