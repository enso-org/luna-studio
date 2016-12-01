{-# LANGUAGE DeriveAnyClass #-}
module Event.UI where

import           Control.DeepSeq         (NFData)
import           Data.Aeson              (FromJSON, ToJSON)
import           Utils.PreludePlus

import qualified React.Event.App         as App
import qualified React.Event.Breadcrumbs as Breadcrumbs
import qualified React.Event.Node        as Node


data UIEvent = NodeEvent               Node.Event
             | AppEvent                 App.Event
             | BreadcrumbsEvent Breadcrumbs.Event
               deriving (Show, Generic, NFData, Typeable)

instance ToJSON   UIEvent
instance FromJSON UIEvent
