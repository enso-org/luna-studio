module Node.Editor.Handler.Sidebar where

import           Node.Editor.Action.Basic        (toggleInputMode, toggleOutputMode)
import           Node.Editor.Action.Command      (Command)
import           Node.Editor.Action.Sidebar      (addPort, cancelPortNameEdit, finishPortNameEdit, handleAppMove, handleMouseUp,
                                                  handleSidebarMove, removePort, startPortNameEdit, unfreezeSidebar)
import           Node.Editor.Event.Event         (Event (UI))
import           Node.Editor.Event.UI            (UIEvent (AppEvent, SidebarEvent))
import           Luna.Prelude
import qualified Node.Editor.React.Event.App     as App
import qualified Node.Editor.React.Event.Sidebar as Sidebar
import           Node.Editor.React.Model.Port    (AnyPortRef (OutPortRef'))
import           Node.Editor.State.Action        (Action (continue))
import           Node.Editor.State.Global        (State)


handle :: Event -> Maybe (Command State ())
handle (UI (SidebarEvent (Sidebar.ToggleInputMode   nl)))                         = Just $ toggleInputMode nl
handle (UI (SidebarEvent (Sidebar.ToggleOutputMode  nl)))                         = Just $ toggleOutputMode nl
handle (UI (SidebarEvent (Sidebar.RemovePort        (OutPortRef' portRef))))      = Just $ removePort portRef
handle (UI (SidebarEvent (Sidebar.AddPort           (OutPortRef' portRef))))      = Just $ addPort portRef
handle (UI (SidebarEvent (Sidebar.UnfreezeSidebar   nodeLoc)))                    = Just $ unfreezeSidebar nodeLoc
handle (UI (AppEvent     (App.MouseMove             evt _)))                      = Just $ handleAppMove evt
handle (UI (SidebarEvent (Sidebar.MouseMove         evt nodeLoc)))                = Just $ handleSidebarMove evt nodeLoc
handle (UI (AppEvent     (App.MouseUp               evt)))                        = Just $ continue $ handleMouseUp evt
handle (UI (SidebarEvent (Sidebar.PortNameStartEdit (OutPortRef' portRef))))      = Just $ startPortNameEdit  portRef
handle (UI (SidebarEvent (Sidebar.PortNameApply     (OutPortRef' portRef) name))) = Just $ finishPortNameEdit portRef name
handle (UI (SidebarEvent (Sidebar.PortNameDiscard   (OutPortRef' portRef))))      = Just $ cancelPortNameEdit portRef
handle _                                                                          = Nothing
