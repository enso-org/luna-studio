module NodeEditor.Handler.Sidebar where

import           NodeEditor.Action.Basic        (toggleInputMode, toggleOutputMode)
import           NodeEditor.Action.Command      (Command)
import           NodeEditor.Action.Sidebar      (addPort, cancelPortNameEdit, finishPortNameEdit, handleAppMove, handleMouseUp,
                                                  handleSidebarMove, removePort, startPortNameEdit)
import           NodeEditor.Event.Event         (Event (UI))
import           NodeEditor.Event.UI            (UIEvent (AppEvent, SidebarEvent))
import           Common.Prelude
import qualified NodeEditor.React.Event.App     as App
import qualified NodeEditor.React.Event.Sidebar as Sidebar
import           NodeEditor.React.Model.Port    (AnyPortRef (OutPortRef'))
import           NodeEditor.State.Action        (Action (continue))
import           NodeEditor.State.Global        (State)


handle :: Event -> Maybe (Command State ())
handle (UI (SidebarEvent (Sidebar.ToggleInputMode   nl)))                         = Just $ toggleInputMode nl
handle (UI (SidebarEvent (Sidebar.ToggleOutputMode  nl)))                         = Just $ toggleOutputMode nl
handle (UI (SidebarEvent (Sidebar.RemovePort        (OutPortRef' portRef))))      = Just $ removePort portRef
handle (UI (SidebarEvent (Sidebar.AddPort           (OutPortRef' portRef))))      = Just $ addPort portRef
handle (UI (AppEvent     (App.MouseMove             evt _)))                      = Just $ handleAppMove evt
handle (UI (SidebarEvent (Sidebar.MouseMove         evt nodeLoc)))                = Just $ handleSidebarMove evt nodeLoc
handle (UI (AppEvent     (App.MouseUp               evt)))                        = Just $ continue $ handleMouseUp evt
handle (UI (SidebarEvent (Sidebar.PortNameStartEdit (OutPortRef' portRef))))      = Just $ startPortNameEdit  portRef
handle (UI (SidebarEvent (Sidebar.PortNameApply     (OutPortRef' portRef) name))) = Just $ finishPortNameEdit portRef name
handle (UI (SidebarEvent (Sidebar.PortNameDiscard   (OutPortRef' portRef))))      = Just $ cancelPortNameEdit portRef
handle _                                                                          = Nothing
