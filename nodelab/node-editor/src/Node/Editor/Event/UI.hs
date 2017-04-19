{-# LANGUAGE DeriveAnyClass #-}
module Node.Editor.Event.UI where

import           Data.Aeson                            (FromJSON, ToJSON)
import           Luna.Prelude

import qualified Node.Editor.React.Event.App           as App
import qualified Node.Editor.React.Event.Breadcrumbs   as Breadcrumbs
import qualified Node.Editor.React.Event.CodeEditor    as CodeEditor
import qualified Node.Editor.React.Event.Connection    as Connection
import qualified Node.Editor.React.Event.Node          as Node
import qualified Node.Editor.React.Event.NodeEditor    as NodeEditor
import qualified Node.Editor.React.Event.Port          as Port
import qualified Node.Editor.React.Event.Searcher      as Searcher
import qualified Node.Editor.React.Event.Sidebar       as Sidebar
import qualified Node.Editor.React.Event.Visualization as Visualization


data UIEvent = AppEvent           App.Event
             | BreadcrumbsEvent   Breadcrumbs.Event
             | CodeEditorEvent    CodeEditor.Event
             | ConnectionEvent    Connection.Event
             | NodeEditorEvent    NodeEditor.Event
             | NodeEvent          Node.Event
             | PortEvent          Port.Event
             | SearcherEvent      Searcher.Event
             | SidebarEvent       Sidebar.Event
             | VisualizationEvent Visualization.Event
               deriving (Show, Generic, NFData, Typeable)

instance ToJSON   UIEvent
instance FromJSON UIEvent
