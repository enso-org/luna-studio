{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Action.ConnectionPen
    ( startConnecting
    , startDisconnecting
    , connectMove
    , disconnectMove
    , resetPenConnectState
    , resetPenDisconnectState
    ) where

import           Luna.Studio.Action.Command       (Command)
import           Luna.Studio.Event.Mouse          (workspacePosition)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Action         (Action (PenConnect, PenDisconnect))
import           Luna.Studio.State.Global         (State)
import qualified Luna.Studio.State.Global         as Global
import qualified Luna.Studio.State.PenConnect     as PenConnect
import qualified Luna.Studio.State.PenDisconnect  as PenDisconnect
import           Luna.Studio.State.StatefulAction (StatefulAction (exit, matchState, pack, start, update))
import           React.Flux                       (MouseEvent)

instance StatefulAction PenConnect.State where
    matchState (PenConnect state) = Just state
    matchState _ = Nothing
    pack = PenConnect
    exit _ = Global.performedAction .= Nothing

instance StatefulAction PenDisconnect.State where
    matchState (PenDisconnect state) = Just state
    matchState _ = Nothing
    pack = PenDisconnect
    exit _ = Global.performedAction .= Nothing

startConnecting :: MouseEvent -> Command State ()
startConnecting evt = do
    pos <- workspacePosition evt
    start $ PenConnect.State [pos] Nothing

startDisconnecting :: MouseEvent -> Command State ()
startDisconnecting evt = do
    pos <- workspacePosition evt
    start $ PenDisconnect.State [pos] Nothing

connectMove :: MouseEvent -> PenConnect.State -> Command State ()
connectMove evt state = do
    pos <- workspacePosition evt
    update $ state & PenConnect.history %~ (pos:)

disconnectMove :: MouseEvent -> PenDisconnect.State -> Command State ()
disconnectMove evt state = do
    pos <- workspacePosition evt
    update $ state & PenDisconnect.history %~ (pos:)

resetPenConnectState :: PenConnect.State -> Command State ()
resetPenConnectState _ = Global.performedAction .= Nothing

resetPenDisconnectState :: PenDisconnect.State -> Command State ()
resetPenDisconnectState _ = Global.performedAction .= Nothing










-- toAction (Mouse _ (Mouse.Event Mouse.Pressed  pos Mouse.LeftButton  (KeyMods False True False False) _)) = Just $ startConnecting pos
-- toAction (Mouse _ (Mouse.Event Mouse.Pressed  pos Mouse.LeftButton  (KeyMods True  True False False) _)) = Just $ startDisconnecting pos
-- toAction (Mouse _ (Mouse.Event Mouse.Pressed  pos Mouse.RightButton (KeyMods False True False False) _)) = Just $ startDisconnecting pos
-- toAction (Mouse _ (Mouse.Event Mouse.Moved    pos Mouse.LeftButton  _ _)) = Just $ whileDrawing $ handleMove pos
-- toAction (Mouse _ (Mouse.Event Mouse.Moved    pos Mouse.RightButton _ _)) = Just $ whileDrawing $ handleMove pos
-- toAction (Mouse _ (Mouse.Event Mouse.Moved    _   Mouse.NoButton    _ _)) = Just stopDrag
-- toAction (Mouse _ (Mouse.Event Mouse.Released _   Mouse.LeftButton  _ _)) = Just stopDrag
-- toAction (Mouse _ (Mouse.Event Mouse.Released _   Mouse.RightButton _ _)) = Just stopDrag
-- toAction (ConnectionPen (ConnectionPen.Segment widgets)) = Just $ whileDrawing $ handleAction widgets

-- startConnecting :: Vector2 Int -> Command State ()
-- startConnecting coord = do
--     performIO $ UI.beginPath coord True
--     Global.connectionPen . ConnectionPen.drawing ?= ConnectionPen.Drawing coord ConnectionPen.Connecting Nothing []
--
-- startDisconnecting :: Vector2 Int -> Command State ()
-- startDisconnecting coord = do
--     performIO $ UI.beginPath coord False
--     Global.connectionPen . ConnectionPen.drawing ?= ConnectionPen.Drawing coord ConnectionPen.Disconnecting Nothing []
--
-- whileDrawing :: (ConnectionPen.Drawing -> Command State ()) -> Command State ()
-- whileDrawing run = do
--     drawingMay <- use $ Global.connectionPen . ConnectionPen.drawing
--     withJust drawingMay $ \drawing -> run drawing
--
-- handleMove :: Vector2 Int -> ConnectionPen.Drawing -> Command State ()
-- handleMove coord drawing = do
--     let previousPos = drawing ^. ConnectionPen.previousPos
--     Global.connectionPen . ConnectionPen.drawing . _Just . ConnectionPen.previousPos .= coord
--     performIO $ do
--         UI.clearCanvas
--         UI.drawSegment coord
--         UI.requestWidgetsBetween previousPos coord
--
-- stopDrag :: Command State ()
-- stopDrag = do
--     Global.connectionPen . ConnectionPen.drawing .= Nothing
--     performIO UI.endPath
--
-- lookupNode :: WidgetId -> Command State (Maybe (WidgetFile UINode.Node))
-- lookupNode = zoom Global.uiRegistry . UIRegistry.lookupTypedM
--
-- lookupConnection :: WidgetId -> Command State (Maybe (WidgetFile UIConnection.Connection))
-- lookupConnection = zoom Global.uiRegistry . UIRegistry.lookupTypedM
--
-- handleAction :: [WidgetId] -> ConnectionPen.Drawing -> Command State ()
-- handleAction widgets drawing = case drawing ^. ConnectionPen.drawingType of
--     ConnectionPen.Connecting    -> handleConnectAction    widgets drawing
--     ConnectionPen.Disconnecting -> handleDisconnectAction widgets
--
-- handleConnectAction :: [WidgetId] -> ConnectionPen.Drawing -> Command State ()
-- handleConnectAction widgets drawing = do
--     nodesMay <- sequence $ lookupNode <$> widgets
--     let nodes = view (widget . UINode.nodeId) <$> catMaybes nodesMay
--     unless (null nodes) $ do
--         let path              = remdups $ maybeToList (drawing ^. ConnectionPen.lastNode) ++ nodes
--         Global.connectionPen . ConnectionPen.drawing . _Just . ConnectionPen.visitedNodes %= (++ nodes)
--         Global.connectionPen . ConnectionPen.drawing . _Just . ConnectionPen.lastNode .= maybeLast path
--         let nodesToConnect    = zipAdj path
--         autoConnectAll nodesToConnect
--
-- handleDisconnectAction :: [WidgetId] -> Command State ()
-- handleDisconnectAction widgets = do
--     connectionsMay <- sequence $ lookupConnection <$> widgets
--     let connections = view (widget . UIConnection.connectionId) <$> catMaybes connectionsMay
--     unless (null connections) $ do
--         removeConnections connections
--         GA.sendEvent GA.Disconnect
--
-- remdups               :: (Eq a) => [a] -> [a]
-- remdups (x : xx : xs) =  if x == xx then remdups (x : xs) else x : remdups (xx : xs)
-- remdups xs            = xs
--
-- zipAdj :: [b] -> [(b, b)]
-- zipAdj x = zip x $ tail x
--
-- maybeLast :: [r] -> Maybe r
-- maybeLast [] = Nothing
-- maybeLast xs = Just $ last xs
--
-- autoConnectAll :: [(NodeId, NodeId)] -> Command State ()
-- autoConnectAll []    = return ()
-- autoConnectAll nodes = autoConnectForward (head nodes) -- TODO: forall - foldr
--
-- autoConnectForward :: (NodeId, NodeId) -> Command State ()
-- autoConnectForward (srcNodeId, dstNodeId) = autoConnect (srcNodeId, dstNodeId)
--
-- autoConnect :: (NodeId, NodeId) -> Command State ()
-- autoConnect (srcNodeId, dstNodeId) = do
--     BatchCmd.connectNodes (OutPortRef srcNodeId Port.All) (InPortRef dstNodeId Port.Self)
--     GA.sendEvent $ GA.Connect GA.Pen
