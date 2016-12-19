{-# LANGUAGE DeriveAnyClass #-}

module Luna.Studio.React.Event.Node where

import           Control.DeepSeq          (NFData)
import           Data.Aeson               (FromJSON, ToJSON)
import           React.Flux               (MouseEvent)

import           Empire.API.Data.Node     (NodeId)
import           Empire.API.Data.Port     (PortId)
import           Empire.API.JSONInstances ()
import           Luna.Studio.Prelude



data Event = DisplayResultChanged Bool NodeId
           | EditExpression       NodeId
           | EndConnection        NodeId PortId
           | Enter                NodeId
           | MouseDown            MouseEvent NodeId
           | Select               MouseEvent NodeId
           | StartConnection      NodeId PortId
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
