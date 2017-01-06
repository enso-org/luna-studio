{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Port where

import           Luna.Studio.Prelude

import           Empire.API.Data.Node               (NodeId)
import           Empire.API.Data.Port               (InPort (..), OutPort (..), PortId (..))
import qualified Event.UI                           as UI
import           Luna.Studio.Data.Angle             (Angle)
import           Luna.Studio.Data.Color             (Color, toJSString)
import qualified Luna.Studio.React.Event.Connection as Connection
import           Luna.Studio.React.Model.Node       (Node)
import           Luna.Studio.React.Model.Port       (Port (..))
import qualified Luna.Studio.React.Model.Port       as Port
import           Luna.Studio.React.Store            (Ref, dispatch)
import           Luna.Studio.React.View.Global
import           React.Flux                         hiding (view)
import qualified React.Flux                         as React



name :: JSString
name = "port"


port :: Ref Node -> Int -> Bool -> ReactView Port
port nodeRef numOfPorts isOnly = React.defineView name $ \p -> do
    drawPort_ nodeRef p numOfPorts isOnly

portExpanded :: Ref Node -> ReactView Port
portExpanded nodeRef = React.defineView name $ \p -> do
    drawPortExpanded_ nodeRef p


port_ :: Ref Node -> Port -> Int -> Bool -> ReactElementM ViewEventHandler ()
port_ ref p numOfPorts isOnly = React.viewWithSKey (port ref numOfPorts isOnly) (fromString $ show $ p ^. Port.portId) p mempty

portExpanded_ :: Ref Node -> Port -> ReactElementM ViewEventHandler ()
portExpanded_ ref p = React.viewWithSKey (portExpanded ref) (fromString $ show $ p ^. Port.portId) p mempty


drawPort_ :: Ref Node -> Port -> Int -> Bool -> ReactElementM ViewEventHandler ()
drawPort_ nodeRef port numOfPorts isOnly = do
    case port ^. Port.portId of
        InPortId   Self          ->                drawPortSelf_   nodeRef port
        OutPortId  All           -> if isOnly then drawPortSingle_ nodeRef port
                                    else           drawPortIO_     nodeRef port 0 numOfPorts False
        InPortId  (Arg        i) ->                drawPortIO_     nodeRef port i numOfPorts True
        OutPortId (Projection i) ->                drawPortIO_     nodeRef port i numOfPorts False

drawPortExpanded_ :: Ref Node -> Port -> ReactElementM ViewEventHandler ()
drawPortExpanded_ nodeRef port = do
    case port ^. Port.portId of
        InPortId   Self          -> drawPortSelf_       nodeRef port
        OutPortId  All           -> drawPortIOExpanded_ nodeRef port 0 False
        InPortId  (Arg        i) -> drawPortIOExpanded_ nodeRef port i True
        OutPortId (Projection i) -> drawPortIOExpanded_ nodeRef port i False

drawPortSelf_ :: Ref Node -> Port -> ReactElementM ViewEventHandler ()
drawPortSelf_ nodeRef port =
    let portRef = port ^. Port.portRef
        color   = toJSString $ port ^. Port.color
        portId  = port ^. Port.portId
    in
    circle_
        [ onMouseDown $ \e m -> stopPropagation e : dispatch nodeRef (UI.ConnectionEvent $ Connection.StartConnection m portRef)
        , onMouseUp   $ \e m -> stopPropagation e : dispatch nodeRef (UI.ConnectionEvent $ Connection.EndConnection   m portRef)
        , "className" $= "port port--self"
        , "key"       $= fromString (show portId)
        , "fill"      $= color
        , "stroke"    $= color
        ] mempty

drawPortSingle_ :: Ref Node -> Port -> ReactElementM ViewEventHandler ()
drawPortSingle_ nodeRef port = do

    let portRef     = port ^. Port.portRef
        portId      = port ^. Port.portId
        color       = toJSString $ port ^. Port.color
        r1          = show nodeRadius
        r2          = show nodeRadius'
        svgPath a b = fromString $ "M0 -" <> r1 <> " A " <> r1 <> " " <> r1 <> " 1 0 " <> show a <> " 0 "  <> r1 <>
                                   " L0 " <> r2 <> " A " <> r2 <> " " <> r2 <> " 1 0 " <> show b <> " 0 -" <> r2 <> " Z "
    path_
        [ onMouseDown $ \e m -> stopPropagation e : dispatch nodeRef (UI.ConnectionEvent $ Connection.StartConnection m portRef)
        , onMouseUp   $ \e m -> stopPropagation e : dispatch nodeRef (UI.ConnectionEvent $ Connection.EndConnection   m portRef)
        , "className" $= "port port--o--single"
        , "key"       $= fromString (show portId)
        , "fill"      $= color
        , "stroke"    $= color
        , "d"         $= (svgPath 0 1 <> svgPath 1 0)
        ] mempty


drawPortIO_ :: Ref Node -> Port -> Int -> Int -> Bool -> ReactElementM ViewEventHandler ()
drawPortIO_ nodeRef port num numOfPorts isInput = do

    let portRef = port ^. Port.portRef
        portId  = port ^. Port.portId
        color   = toJSString $ port ^. Port.color

        classes  = if isInput then "port port--i port--i--" else "port port--o port--o--"
        svgFlag1 = if isInput then "1" else "0"
        svgFlag2 = if isInput then "0" else "1"
        mod      = if isInput then -1.0 else 1.0

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
        [ onMouseDown $ \e m -> stopPropagation e : dispatch nodeRef (UI.ConnectionEvent $ Connection.StartConnection m portRef)
        , onMouseUp   $ \e m -> stopPropagation e : dispatch nodeRef (UI.ConnectionEvent $ Connection.EndConnection   m portRef)
        , "className" $= (fromString $ classes <> show (num+1))
        , "key"       $= fromString (show portId)
        , "fill"      $= color
        , "stroke"    $= color
        , "d"         $= svgPath
        ] mempty


drawPortIOExpanded_ :: Ref Node -> Port -> Int -> Bool -> ReactElementM ViewEventHandler ()
drawPortIOExpanded_ nodeRef port num isInput = do

    let portRef = port ^. Port.portRef
        portId  = port ^. Port.portId
        color   = toJSString $ port ^. Port.color
        classes = if isInput then "port port--i port--i--" else "port port--o port--o--"
        n       = if isInput then 1 else 0

    circle_
        [ onMouseDown $ \e m -> stopPropagation e : dispatch nodeRef (UI.ConnectionEvent $ Connection.StartConnection m portRef)
        , onMouseUp   $ \e m -> stopPropagation e : dispatch nodeRef (UI.ConnectionEvent $ Connection.EndConnection   m portRef)
        , "className" $= fromString (classes <> show (num + 1) )
        , "key"       $= fromString (show portId <> show num)
        , "fill"      $= color
        , "stroke"    $= color
        , "r"         $= "3"
        , "cy"        $= fromString (showSvg $ lineHeight * fromIntegral (num + n) )
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
