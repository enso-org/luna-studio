{-# LANGUAGE DeriveAnyClass #-}

module Luna.Studio.React.Event.Node where

import           Control.DeepSeq          (NFData)
import           Data.Aeson               (FromJSON, ToJSON)
import           React.Flux               (KeyboardEvent, MouseEvent)

import           Empire.API.Data.Node     (NodeId)
import           Empire.API.Data.Port     (PortId)
import           Empire.API.JSONInstances ()
import           Luna.Studio.Prelude



data Event = DisplayResultChanged Bool NodeId
           | EditExpression       NodeId
           | EndConnection        MouseEvent NodeId PortId
           | Enter                NodeId
           | MouseDown            MouseEvent NodeId
           | Select               MouseEvent NodeId
           | StartConnection      MouseEvent NodeId PortId
           | NameEditStart        NodeId
           | NameKeyDown          KeyboardEvent NodeId
           | NameChange           Text NodeId
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
