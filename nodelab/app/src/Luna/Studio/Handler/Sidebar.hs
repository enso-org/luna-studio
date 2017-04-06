module Luna.Studio.Handler.Sidebar where

import           Luna.Studio.Action.Basic        (toggleSidebarMode)
import           Luna.Studio.Action.Command      (Command)
import           Luna.Studio.Action.Sidebar      (addPort, cancelPortNameEdit, finishPortNameEdit, handleAppMove, handleMouseUp,
                                                  handleSidebarMove, removePort, startPortNameEdit)
import           Luna.Studio.Event.Event         (Event (UI))
import           Luna.Studio.Event.UI            (UIEvent (AppEvent, SidebarEvent))
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.App     as App
import qualified Luna.Studio.React.Event.Sidebar as Sidebar
import           Luna.Studio.State.Action        (Action (continue))
import           Luna.Studio.State.Global        (State)


handle :: Event -> Maybe (Command State ())
handle (UI (SidebarEvent (Sidebar.ToggleSidebarMode nl)))           = Just $ toggleSidebarMode nl
handle (UI (SidebarEvent (Sidebar.RemovePort portRef)))             = Just $ removePort portRef
handle (UI (SidebarEvent (Sidebar.AddPort    portRef)))             = Just $ addPort portRef
handle (UI (AppEvent     (App.MouseMove   evt _)))                  = Just $ handleAppMove evt
handle (UI (SidebarEvent (Sidebar.MouseMove  evt nodeLoc)))         = Just $ handleSidebarMove evt nodeLoc
handle (UI (AppEvent     (App.MouseUp     evt)))                    = Just $ continue $ handleMouseUp evt
handle (UI (SidebarEvent (Sidebar.PortNameStartEdit portRef)))      = Just $ startPortNameEdit  portRef
handle (UI (SidebarEvent (Sidebar.PortNameApply     portRef name))) = Just $ finishPortNameEdit portRef name
handle (UI (SidebarEvent (Sidebar.PortNameDiscard   portRef)))      = Just $ cancelPortNameEdit portRef
handle _                                                            = Nothing
