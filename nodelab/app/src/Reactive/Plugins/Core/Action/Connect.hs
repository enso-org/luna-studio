module Reactive.Plugins.Core.Action.Connect
    ( toAction
    , handleMove
    ) where

import           Utils.Angle                     (boundedAngle, toAngle)
import           Utils.PreludePlus
import           Utils.Vector                    (Vector2 (Vector2), x, y)

import           Object.Widget                   (WidgetFile, parent, widget)
import qualified Object.Widget.Connection        as UIConnection
import qualified Object.Widget.Node              as NodeModel
import qualified Object.Widget.Port              as PortModel

import           Event.Event                     (Event (..))
import           Event.Keyboard                  hiding (Event)
import           Event.Mouse                     hiding (Event, widget)
import qualified Event.Mouse                     as Mouse

import           Reactive.Commands.Command       (Command)
import           Reactive.Commands.Graph         (portRefToWidgetId)
import           Reactive.Commands.Graph.Connect (batchConnectNodes)
import qualified Reactive.Commands.UIRegistry    as UICmd
import qualified Reactive.State.Camera           as Camera
import           Reactive.State.Connect          (Connecting (Connecting))
import qualified Reactive.State.Connect          as Connect
import           Reactive.State.Global           (State, inRegistry)
import qualified Reactive.State.Global           as Global
import qualified Reactive.State.UIRegistry       as UIRegistry

import           Empire.API.Data.Port            (InPort (Self))
import           Empire.API.Data.PortRef         (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import qualified Empire.API.Data.PortRef         as PortRef (dstNodeId, srcNodeId)
import qualified JS.GoogleAnalytics              as GA



toAction :: Event -> Maybe (Command State ())
toAction (Mouse _ (Mouse.Event Mouse.Pressed  _   Mouse.LeftButton (KeyMods False False False False) (Just evWd))) = Just $ startDragFromPort evWd
toAction (Mouse _ (Mouse.Event Mouse.Moved    pos Mouse.LeftButton _ _      )) = Just $ whileConnecting $ handleMove pos
toAction (Mouse _ (Mouse.Event Mouse.Moved    _   _                _ _      )) = Just $ whileConnecting $ stopDrag'
toAction (Mouse _ (Mouse.Event Mouse.Released _   Mouse.LeftButton _ mayEvWd)) = Just $ whileConnecting $ stopDrag mayEvWd
toAction _                                                                     = Nothing

showCurrentConnection :: Vector2 Double -> Vector2 Double -> Bool -> Command UIRegistry.State ()
showCurrentConnection src dst arrow = UICmd.update_ UIRegistry.currentConnectionId $ (UIConnection.currentFrom    .~ src)
                                                                                   . (UIConnection.currentTo      .~ dst)
                                                                                   . (UIConnection.currentVisible .~ True)
                                                                                   . (UIConnection.currentArrow   .~ arrow)

setCurrentConnectionColor :: Int -> Command UIRegistry.State ()
setCurrentConnectionColor color = UICmd.update_ UIRegistry.currentConnectionId $ UIConnection.currentColor .~ color

hideCurrentConnection :: Command UIRegistry.State ()
hideCurrentConnection = UICmd.update_ UIRegistry.currentConnectionId $ UIConnection.currentVisible .~ False

getPortWidgetUnderCursor :: EventWidget -> Command UIRegistry.State (Maybe (WidgetFile PortModel.Port))
getPortWidgetUnderCursor (EventWidget wid _ _) = UIRegistry.lookupTypedM wid

startDragFromPort :: Mouse.EventWidget -> Command State ()
startDragFromPort evWd = do
    sourcePortWd <- zoom Global.uiRegistry $ getPortWidgetUnderCursor evWd
    withJust sourcePortWd $ \file -> do
        let model = file ^. widget
        let sourceRef = model ^. PortModel.portRef
        nodeWidget <- inRegistry $ UICmd.parent $ fromJust $ file ^. parent
        sourceNodePos <- inRegistry $ UICmd.get nodeWidget NodeModel.position
        Global.connect . Connect.connecting ?= Connecting sourceRef (model ^. PortModel.angleVector) sourceNodePos
        zoom Global.uiRegistry $ setCurrentConnectionColor $ model ^. PortModel.color

whileConnecting :: (Connecting -> Command State ()) -> Command State ()
whileConnecting run = do
    connectingMay <- use $ Global.connect . Connect.connecting
    withJust connectingMay $ \connecting -> run connecting

handleMove :: Vector2 Int -> Connecting -> Command State ()
handleMove coord (Connecting sourceRef _ nodePos) = do
    current <- zoom Global.camera $ Camera.screenToWorkspaceM coord
    startLine <- case sourceRef of
            (InPortRef' (InPortRef _ Self)) -> return nodePos
            _                   -> do
                maySourceWidget <- portRefToWidgetId sourceRef
                case maySourceWidget of
                    Just sourceWidget -> inRegistry $ do
                        newVector <- UICmd.get sourceWidget PortModel.angleVector
                        portCount <- UICmd.get sourceWidget PortModel.portCount
                        let
                            portAngle = toAngle $ newVector
                            angle     = boundedAngle portAngle portCount nodePos current
                            sx        = (nodePos ^. x) + outerPos * cos angle
                            sy        = (nodePos ^. y) + outerPos * sin angle
                            outerPos  = 22.0
                        return $ Vector2 sx sy
                    Nothing -> return nodePos
    inRegistry $ case sourceRef of
        InPortRef'   (InPortRef _ Self) -> showCurrentConnection current startLine False
        InPortRef'   (InPortRef _ _)    -> showCurrentConnection current startLine True
        OutPortRef'  _                  -> showCurrentConnection startLine current True

stopDrag' :: Connect.Connecting -> Command State ()
stopDrag' _ = do
    Global.connect . Connect.connecting .= Nothing
    zoom Global.uiRegistry hideCurrentConnection

toValidConnection :: AnyPortRef -> AnyPortRef -> Maybe (OutPortRef, InPortRef)
toValidConnection src' dst' = (normalize src' dst') >>= toOtherNode where
    normalize (OutPortRef' a) (InPortRef' b) = Just (a, b)
    normalize (InPortRef' a) (OutPortRef' b) = Just (b, a)
    normalize _ _ = Nothing
    toOtherNode (a, b)
        | a ^. PortRef.srcNodeId /= b ^. PortRef.dstNodeId = Just (a, b)
        | otherwise                                        = Nothing

stopDrag :: Maybe Mouse.EventWidget -> Connect.Connecting -> Command State ()
stopDrag mayEvWd (Connecting sourceRef _ _) = do
    Global.connect . Connect.connecting .= Nothing
    zoom Global.uiRegistry hideCurrentConnection
    withJust mayEvWd $ \evWd -> do
        destinationFile <- zoom Global.uiRegistry $ getPortWidgetUnderCursor evWd
        withJust destinationFile $ \destinationFile' -> do
            let destinationRef = destinationFile' ^. widget . PortModel.portRef
            let srcDstMay = toValidConnection sourceRef destinationRef
            withJust srcDstMay $ \(src, dst) -> do
                batchConnectNodes src dst
                GA.sendEvent $ GA.Connect GA.Manual
