{-# LANGUAGE DeriveAnyClass #-}
module NodeEditor.Event.UI where

import           Common.Prelude

import qualified NodeEditor.React.Event.App           as App
import qualified NodeEditor.React.Event.Breadcrumbs   as Breadcrumbs
import qualified NodeEditor.React.Event.Connection    as Connection
import qualified NodeEditor.React.Event.Node          as Node
import qualified NodeEditor.React.Event.NodeEditor    as NodeEditor
import qualified NodeEditor.React.Event.Port          as Port
import qualified NodeEditor.React.Event.Searcher      as Searcher
import qualified NodeEditor.React.Event.Sidebar       as Sidebar
import qualified NodeEditor.React.Event.Visualization as Visualization


data UIEvent = AppEvent           App.Event
             | BreadcrumbsEvent   Breadcrumbs.Event
             | ConnectionEvent    Connection.Event
             | NodeEditorEvent    NodeEditor.Event
             | NodeEvent          Node.Event
             | PortEvent          Port.Event
             | SearcherEvent      Searcher.Event
             | SidebarEvent       Sidebar.Event
             | VisualizationEvent Visualization.Event
             deriving (Generic, NFData, Show, Typeable)
