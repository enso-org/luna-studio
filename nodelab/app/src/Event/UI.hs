{-# LANGUAGE DeriveAnyClass #-}
module Event.UI where

import           Control.DeepSeq       (NFData)
import           Data.Aeson            (FromJSON, ToJSON)
import           Utils.PreludePlus

import qualified React.Store.NodeEvent as Node



data UIEvent = NodeEvent Node.Event
               deriving (Show, Generic, NFData, Typeable)

instance ToJSON   UIEvent
instance FromJSON UIEvent
