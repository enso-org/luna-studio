{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.Event.UI where

import           Common.Prelude

import           Common.Analytics                     (IsTrackedEvent (..))
import           Common.Data.Event                    (EventName (eventName), consName)
import qualified NodeEditor.React.Event.App           as App
import qualified NodeEditor.React.Event.Breadcrumbs   as Breadcrumbs
import qualified NodeEditor.React.Event.Connection    as Connection
import qualified NodeEditor.React.Event.Node          as Node
import qualified NodeEditor.React.Event.Port          as Port
import qualified NodeEditor.React.Event.Searcher      as Searcher
import qualified NodeEditor.React.Event.Sidebar       as Sidebar
import qualified NodeEditor.React.Event.Visualization as Visualization


data UIEvent = AppEvent           App.Event
             | BreadcrumbsEvent   Breadcrumbs.Event
             | ConnectionEvent    Connection.Event
             | NodeEvent          Node.Event
             | PortEvent          Port.Event
             | SearcherEvent      Searcher.Event
             | SidebarEvent       Sidebar.Event
             | VisualizationEvent Visualization.Event
             deriving (Generic, NFData, Show, Typeable)

instance EventName UIEvent where
    eventName event = consName event <> "." <> case event of
        AppEvent           ev -> eventName ev
        BreadcrumbsEvent   ev -> eventName ev
        ConnectionEvent    ev -> eventName ev
        NodeEvent          ev -> eventName ev
        PortEvent          ev -> eventName ev
        SearcherEvent      ev -> eventName ev
        SidebarEvent       ev -> eventName ev
        VisualizationEvent ev -> eventName ev

instance IsTrackedEvent UIEvent where
    isTracked _ = False
