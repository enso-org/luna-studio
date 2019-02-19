module NodeEditor.Handler.Sidebar where

import           Common.Action.Command             (Command)
import           Common.Prelude
import qualified NodeEditor.Action.Batch           as Batch
import           NodeEditor.Action.Searcher        (editPortName)
import           NodeEditor.Action.Sidebar         (escapeAddRemoveMode, handleAppMove, handleMouseUp, handleSidebarMove, toggleSidebarMode)
import           NodeEditor.Event.Event            (Event (UI))
import           NodeEditor.Event.UI               (UIEvent (AppEvent, ConnectionEvent, NodeEvent, PortEvent, SearcherEvent, SidebarEvent, VisualizationEvent))
import qualified NodeEditor.React.Event.App        as App
import qualified NodeEditor.React.Event.Connection as Connection
import qualified NodeEditor.React.Event.Node       as Node
import qualified NodeEditor.React.Event.Port       as Port
import qualified NodeEditor.React.Event.Sidebar    as Sidebar
import           NodeEditor.React.Model.Port       (AnyPortRef (OutPortRef'))
import           NodeEditor.State.Action           (Action (continue))
import           NodeEditor.State.Global           (State)


handle :: Event -> Maybe (Command State ())
handle (UI (SidebarEvent       (Sidebar.ToggleInputMode  nl)))                = Just $ toggleSidebarMode nl
handle (UI (SidebarEvent       (Sidebar.ToggleOutputMode nl)))                = Just $ toggleSidebarMode nl
handle (UI (AppEvent           (App.MouseDown            _ _)))               = Just $ continue escapeAddRemoveMode
handle (UI (ConnectionEvent    (Connection.MouseDown     _ _ _)))             = Just $ continue escapeAddRemoveMode
handle (UI (NodeEvent          (Node.Event _ (Node.MouseDown _))))            = Just $ continue escapeAddRemoveMode
handle (UI (PortEvent          (Port.MouseDown           _ _)))               = Just $ continue escapeAddRemoveMode
handle (UI (SearcherEvent      {}))                                           = Just $ continue escapeAddRemoveMode
handle (UI (VisualizationEvent {}))                                           = Just $ continue escapeAddRemoveMode
handle (UI (SidebarEvent       (Sidebar.RemovePort   (OutPortRef' portRef)))) = Just $ Batch.removePort portRef
handle (UI (SidebarEvent       (Sidebar.AddPort      (OutPortRef' portRef)))) = Just $ Batch.addPort portRef Nothing def
handle (UI (AppEvent           (App.MouseMove        evt _)))                 = Just $ handleAppMove evt
handle (UI (SidebarEvent       (Sidebar.MouseMove    evt nodeLoc _)))         = Just $ handleSidebarMove evt nodeLoc
handle (UI (AppEvent           (App.MouseUp          evt)))                   = Just $ continue $ handleMouseUp evt
handle (UI (SidebarEvent       (Sidebar.EditPortName portRef)))               = Just $ editPortName portRef
handle _                                                                      = Nothing
