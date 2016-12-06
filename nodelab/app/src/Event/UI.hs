{-# LANGUAGE DeriveAnyClass #-}
module Event.UI where

import           Control.DeepSeq          (NFData)
import           Data.Aeson               (FromJSON, ToJSON)
import           Utils.PreludePlus

import qualified React.Event.App          as App
import qualified React.Event.Breadcrumbs  as Breadcrumbs
import qualified React.Event.Node         as Node
import qualified React.Event.NodeEditor   as NodeEditor
import qualified React.Event.NodeSearcher as NodeSearcher


data UIEvent = AppEvent          App.Event
             | BreadcrumbsEvent  Breadcrumbs.Event
             | NodeEvent         Node.Event
             | NodeEditorEvent   NodeEditor.Event
             | NodeSearcherEvent NodeSearcher.Event
               deriving (Show, Generic, NFData, Typeable)

instance ToJSON   UIEvent
instance FromJSON UIEvent
