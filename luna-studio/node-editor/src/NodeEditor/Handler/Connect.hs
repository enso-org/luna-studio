module NodeEditor.Handler.Connect
    ( handle
    ) where

import           Common.Action.Command             (Command)
import           Common.Prelude
import           LunaStudio.Data.PortRef           (AnyPortRef (InPortRef', OutPortRef'))
import           NodeEditor.Action.Connect         (cancelSnapToPort, handleConnectionMouseDown, handleMouseUp, handleMove,
                                                    handlePortMouseUp, snapToPort)
import           NodeEditor.Event.Event            (Event (UI, View))
import           NodeEditor.Event.UI               (UIEvent (AppEvent, ConnectionEvent, PortEvent, SidebarEvent))
import           NodeEditor.Event.View             (BaseEvent (Disconnect, Mouse), ViewEvent, base)
import qualified NodeEditor.Event.View             as View
import qualified NodeEditor.React.Event.App        as App
import           NodeEditor.React.Event.Connection (ModifiedEnd (Destination, Source))
import qualified NodeEditor.React.Event.Connection as Connection
import qualified NodeEditor.React.Event.Port       as Port
import qualified NodeEditor.React.Event.Sidebar    as Sidebar
import           NodeEditor.State.Action           (Action (continue, end), Connect)
import           NodeEditor.State.Global           (State)
import           NodeEditor.State.Mouse            (mousePosition, workspacePosition)

handle :: Event -> Maybe (Command State ())
handle (UI   evt) = handleUIEvent evt
handle (View evt) = handleViewEvent evt
handle _          = Nothing


handleUIEvent :: UIEvent -> Maybe (Command State ())
handleUIEvent (AppEvent (App.MouseMove evt _))
    = Just $ continue . handleMove =<< workspacePosition evt
handleUIEvent (AppEvent (App.MouseUp evt))
    = Just $ continue . handleMouseUp =<< mousePosition evt
handleUIEvent (AppEvent App.Click)
    = Just $ continue (end :: Connect -> Command State ())
handleUIEvent (PortEvent (Port.MouseUp    portRef))
    = Just $ continue $ handlePortMouseUp portRef
handleUIEvent (PortEvent (Port.MouseEnter portRef))
    = Just $ continue $ snapToPort portRef
handleUIEvent (PortEvent (Port.MouseLeave portRef))
    = Just $ continue $ cancelSnapToPort portRef
handleUIEvent (SidebarEvent (Sidebar.MouseMove evt _ _))
    = Just $ continue . handleMove =<< workspacePosition evt
handleUIEvent (ConnectionEvent (Connection.MouseDown evt connId end')) = Just
    $ mousePosition evt >>= \mp -> handleConnectionMouseDown mp connId end'
handleUIEvent _ = Nothing

handleViewEvent :: ViewEvent -> Maybe (Command State ())
handleViewEvent evt = case evt ^. base of
    Disconnect e -> Just $ do
        let inPortRef = View.getInPortRef evt
            connectionEnd = if e ^. View.src then Source else Destination
        handleConnectionMouseDown def inPortRef connectionEnd
    Mouse e -> do
        let path = evt ^. View.path
            isInPort  = last path == "InPort"
            isOutPort = last path == "OutPort"
            anyPortRef = if isInPort
                then InPortRef'  $ View.getInPortRef evt
                else OutPortRef' $ View.getOutPortRef evt
        if isInPort || isOutPort then case e ^. View.type_ of
            "mouseup"     -> Just . continue $ handlePortMouseUp anyPortRef
            "mouseenter"  -> Just . continue $ snapToPort anyPortRef
            "mouseleave"  -> Just . continue $ cancelSnapToPort anyPortRef
            _ -> Nothing
        else if path == ["NodeEditor"] then case e ^. View.type_ of
            "mouseup" -> Just . continue
                . handleMouseUp . View.mousePosition $ evt ^. base
            "click"   -> Just $ continue (end :: Connect -> Command State ())
            _         -> Nothing
        else Nothing
    _ -> Nothing
