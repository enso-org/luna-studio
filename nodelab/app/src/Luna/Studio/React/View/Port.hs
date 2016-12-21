{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Port where

import           Luna.Studio.Prelude

import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.Port          (InPort (..), OutPort (..), PortId (..))
import qualified Event.UI                      as UI
import           Luna.Studio.Data.Angle        (Angle)
import           Luna.Studio.Data.Color        (Color (Color))
import           Luna.Studio.Data.HSL          (color')
import qualified Luna.Studio.React.Event.Node  as Node
import           Luna.Studio.React.Model.Node  (Node)
import           Luna.Studio.React.Store       (Ref, dispatch)
import           Luna.Studio.React.View.Global
import           Object.Widget.Port            (Port (..))
import           React.Flux                    hiding (view)
import qualified React.Flux                    as React



name :: JSString
name = "port"


port :: Ref Node -> NodeId -> Int -> Bool -> ReactView Port
port nodeRef nodeId numOfPorts isOnly = React.defineView name $ \p -> do
    drawPort_ nodeRef nodeId p numOfPorts isOnly

port_ :: Ref Node -> NodeId -> Port -> Int -> Bool -> ReactElementM ViewEventHandler ()
port_ ref nodeId p numOfPorts isOnly = React.view (port ref nodeId numOfPorts isOnly) p mempty


drawPort_ :: Ref Node -> NodeId -> Port -> Int -> Bool -> ReactElementM ViewEventHandler ()
drawPort_ nodeRef nodeId (Port _ portId@(InPortId   Self         ) _) _          _     = drawPortSelf_   nodeRef nodeId portId
drawPort_ nodeRef nodeId (Port _ portId@(OutPortId  All          ) _) _          True  = drawPortSingle_ nodeRef nodeId portId
drawPort_ nodeRef nodeId (Port _ portId@(OutPortId  All          ) _) numOfPorts False = drawPortIO_     nodeRef nodeId portId 0 numOfPorts False
drawPort_ nodeRef nodeId (Port _ portId@(InPortId  (Arg        i)) _) numOfPorts _     = drawPortIO_     nodeRef nodeId portId i numOfPorts True
drawPort_ nodeRef nodeId (Port _ portId@(OutPortId (Projection i)) _) numOfPorts _     = drawPortIO_     nodeRef nodeId portId i numOfPorts False


drawPortSelf_ :: Ref Node -> NodeId -> PortId -> ReactElementM ViewEventHandler ()
drawPortSelf_ nodeRef nodeId portId = let color = color' $ Color 5 in
    circle_
        [ onMouseDown $ \e m -> stopPropagation e : dispatch nodeRef (UI.NodeEvent $ Node.StartConnection m nodeId portId)
        , onMouseUp   $ \e m -> stopPropagation e : dispatch nodeRef (UI.NodeEvent $ Node.EndConnection   m nodeId portId)
        , "className" $= "port port--self"
        , "fill"      $= color
        , "stroke"    $= color
        ] mempty


drawPortSingle_ :: Ref Node -> NodeId -> PortId -> ReactElementM ViewEventHandler ()
drawPortSingle_ nodeRef nodeId portId = do

    let
        color = color' $ Color 5
        r1 = show nodeRadius
        r2 = show nodeRadius'
        svgPath a b = fromString $ "M0 -" <> r1 <> " A " <> r1 <> " " <> r1 <> " 1 0 " <> show a <> " 0 "  <> r1 <>
                                   " L0 " <> r2 <> " A " <> r2 <> " " <> r2 <> " 1 0 " <> show b <> " 0 -" <> r2 <> " Z "
    path_
        [ onMouseDown $ \e m -> stopPropagation e : dispatch nodeRef (UI.NodeEvent $ Node.StartConnection m nodeId portId)
        , onMouseUp   $ \e m -> stopPropagation e : dispatch nodeRef (UI.NodeEvent $ Node.EndConnection   m nodeId portId)
        , "className" $= "port port--o--single"
        , "fill"      $= color
        , "stroke"    $= color
        , "d"         $= (svgPath 0 1 <> svgPath 1 0)
        ] mempty


drawPortIO_ :: Ref Node -> NodeId -> PortId -> Int -> Int -> Bool -> ReactElementM ViewEventHandler ()
drawPortIO_ nodeRef nodeId portId num numOfPorts isInput = do

    let color    = color' $ Color 5 --TODO [Piotr Młodawski]: get color from model

        classes  = case isInput of True  -> "port port--i port--i--"
                                   False -> "port port--o port--o--"
        svgFlag1 = case isInput of True  -> "0"
                                   False -> "1"
        svgFlag2 = case isInput of True  -> "1"
                                   False -> "0"
        mod      = case isInput of True  ->  1.0
                                   False -> -1.0

        startPortArcX r = r * sin(portAngleStart num numOfPorts r * mod)
        startPortArcY r = r * cos(portAngleStart num numOfPorts r * mod)
        stopPortArcX  r = r * sin(portAngleStop  num numOfPorts r * mod)
        stopPortArcY  r = r * cos(portAngleStop  num numOfPorts r * mod)

        ax = showSvg $ startPortArcX nodeRadius
        ay = showSvg $ startPortArcY nodeRadius
        bx = showSvg $ stopPortArcX  nodeRadius
        by = showSvg $ stopPortArcY  nodeRadius
        cx = showSvg $ stopPortArcX  nodeRadius'
        cy = showSvg $ stopPortArcY  nodeRadius'
        dx = showSvg $ startPortArcX nodeRadius'
        dy = showSvg $ startPortArcY nodeRadius'
        r1 = show nodeRadius
        r2 = show nodeRadius'

        svgPath = fromString $ "M"  <> ax <> " " <> ay <>
                              " A " <> r1 <> " " <> r1 <> " 1 0 " <> svgFlag1 <> " " <> bx <> " " <> by <>
                              " L " <> cx <> " " <> cy <>
                              " A " <> r2 <> " " <> r2 <> " 1 0 " <> svgFlag2 <> " " <> dx <> " " <> dy <>
                              " Z"
    path_
        [ onMouseDown $ \e m -> stopPropagation e : dispatch nodeRef (UI.NodeEvent $ Node.StartConnection m nodeId portId)
        , onMouseUp   $ \e m -> stopPropagation e : dispatch nodeRef (UI.NodeEvent $ Node.EndConnection   m nodeId portId)
        , "className" $= (fromString $ classes <> show (num+1))
        , "fill"      $= color
        , "stroke"    $= color
        , "d"         $= svgPath
        ] mempty


--TODO[react] probably remove
-- displayPorts :: WidgetId -> Node -> Command Global.State ()
-- displayPorts wid node = do
--         portGroup <- inRegistry $ UICmd.get wid $ Model.elements . Model.portGroup
--         oldPorts  <- inRegistry $ UICmd.children portGroup
--         oldPortWidgets <- forM oldPorts $ \wid' -> inRegistry $ (UICmd.lookup wid')
--         let portRefs = (view PortModel.portRef) <$> oldPortWidgets
--         forM_ portRefs $ \wid' -> Global.graph . Graph.portWidgetsMap . at wid' .= Nothing
--         inRegistry $ mapM_ UICmd.removeWidget oldPorts
--
--         groupId      <- inRegistry $ Node.portControlsGroupId wid
--         portControls <- inRegistry $ UICmd.children groupId
--         inRegistry $ mapM_ UICmd.removeWidget portControls
--
--         inLabelsGroupId <- inRegistry $ Node.inLabelsGroupId wid
--         inLabels        <- inRegistry $ UICmd.children inLabelsGroupId
--         inRegistry $ mapM_ UICmd.removeWidget inLabels
--
--         outLabelsGroupId <- inRegistry $ Node.outLabelsGroupId wid
--         outLabels        <- inRegistry $ UICmd.children outLabelsGroupId
--         inRegistry $ mapM_ UICmd.removeWidget outLabels
--
--         forM_ (makePorts node    ) $ \p -> do
--             portWidgetId <- inRegistry $ UICmd.register portGroup p def
--             Global.graph . Graph.portWidgetsMap . at (p ^. PortModel.portRef) ?= portWidgetId
--         inRegistry $ forM_ (node ^. Node.ports) $ makePortControl groupId node
--         inRegistry $ forM_ (node ^. Node.ports) $ \p -> case p ^. Port.portId of
--             InPortId  Self -> return ()
--             InPortId  _    -> makePortLabel inLabelsGroupId  p
--             OutPortId _    -> makePortLabel outLabelsGroupId p

--TODO[react] probably remove
-- makePortLabel :: WidgetId -> Port -> Command UIRegistry.State ()
-- makePortLabel parent port = do
--     let align = case port ^. Port.portId of
--             InPortId  _ -> Label.Right
--             OutPortId _ -> Label.Left
--         label = Label.create (Vector2 360 15) text & Label.alignment .~ align
--         text  = (Text.pack $ port ^. Port.name) <> " :: " <> portType
--         portType = port ^. Port.valueType . ValueType.toText
--     UICmd.register_ parent label def
--
-- TODO[react]: handle this:
-- onMouseOver, onMouseOut :: WidgetId -> Command Global.State ()
-- onMouseOver wid = inRegistry $ do
--     UICmd.update_ wid $ Model.highlight .~ True
--     nodeId <- toNodeId wid
--     Node.showHidePortLabels True nodeId
-- onMouseOut  wid = inRegistry $ do
--     UICmd.update_ wid $ Model.highlight .~ False
--     nodeId <- toNodeId wid
--     Node.showHidePortLabels False nodeId
