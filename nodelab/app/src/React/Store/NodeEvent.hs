{-# LANGUAGE DeriveAnyClass #-}

module React.Store.NodeEvent where

import           Control.DeepSeq          (NFData)
import           Data.Aeson               (FromJSON, ToJSON)

import           Empire.API.Data.Node     (NodeId)
import           Empire.API.JSONInstances ()
import           Utils.PreludePlus



data Event = OnClick
           | Enter NodeId
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
