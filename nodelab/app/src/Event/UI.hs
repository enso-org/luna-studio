{-# LANGUAGE DeriveAnyClass #-}
module Event.UI where

import           Control.DeepSeq         (NFData)
import           Data.Aeson              (FromJSON, ToJSON)
import           Luna.Studio.Prelude

import qualified Luna.Studio.React.Event.App         as App
import qualified Luna.Studio.React.Event.Breadcrumbs as Breadcrumbs
import qualified Luna.Studio.React.Event.CodeEditor  as CodeEditor
import qualified Luna.Studio.React.Event.Node        as Node
import qualified Luna.Studio.React.Event.NodeEditor  as NodeEditor
import qualified Luna.Studio.React.Event.Searcher    as Searcher


data UIEvent = AppEvent          App.Event
             | BreadcrumbsEvent  Breadcrumbs.Event
             | CodeEditorEvent   CodeEditor.Event
             | NodeEditorEvent   NodeEditor.Event
             | NodeEvent         Node.Event
             | SearcherEvent     Searcher.Event
               deriving (Show, Generic, NFData, Typeable)

instance ToJSON   UIEvent
instance FromJSON UIEvent
