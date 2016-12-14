{-# LANGUAGE DeriveAnyClass #-}

module Luna.Studio.React.Event.Node where

import           Control.DeepSeq          (NFData)
import           Data.Aeson               (FromJSON, ToJSON)
import           React.Flux               (MouseEvent)

import           Empire.API.Data.Node     (NodeId)
import           Empire.API.JSONInstances ()
import           Luna.Studio.Prelude



data Event = Select MouseEvent NodeId
           | MouseDown MouseEvent NodeId
           | Enter NodeId
           | EditExpression NodeId
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
