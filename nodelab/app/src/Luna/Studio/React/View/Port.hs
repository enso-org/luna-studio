{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Port where

import           Empire.API.Data.Port               (InPort (..), OutPort (..), PortId (..))
import           Empire.API.Data.PortRef            (AnyPortRef)
import qualified Event.UI                           as UI
import           Luna.Studio.Action.Geometry        (lineHeight, nodeRadius, nodeRadius', portAngleStart, portAngleStop)
import           Luna.Studio.Data.Color             (toJSString)
import qualified Luna.Studio.Event.Mouse            as Mouse
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Connection as Connection
import           Luna.Studio.React.Model.App        (App)
import           Luna.Studio.React.Model.Port       (Port (..))
import qualified Luna.Studio.React.Model.Port       as Port
import           Luna.Studio.React.Store            (Ref, dispatch)
import           Numeric                            (showFFloat)
import           React.Flux                         hiding (view)
import qualified React.Flux                         as React


name :: JSString
name = "port"

show2 :: Double -> String
show2 a = showFFloat (Just 2) a "" -- limit Double to two decimal numbers

handleMouseDown :: Ref App -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseDown ref portRef e m =
    if (Mouse.withoutMods m Mouse.leftButton) then
        stopPropagation e : dispatch ref (UI.ConnectionEvent $ Connection.StartConnection m portRef)
    else []

handleMouseUp :: Ref App -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseUp ref portRef _e m = dispatch ref (UI.ConnectionEvent $ Connection.EndConnection m portRef)

port :: ReactView (Ref App, Int, Bool, Port)
port = React.defineView name $ \(ref, numOfPorts, isOnly, p) ->
    case p ^. Port.portId of
        InPortId   Self          ->                portSelf_   ref p
        OutPortId  All           -> if isOnly then portSingle_ ref p
                                    else           portIO_     ref p 0 numOfPorts False
        InPortId  (Arg        i) ->                portIO_     ref p i numOfPorts True
        OutPortId (Projection i) ->                portIO_     ref p i numOfPorts False

portExpanded :: ReactView (Ref App, Port)
portExpanded = React.defineView name $ \(ref, p) ->
    case p ^. Port.portId of
        InPortId   Self          -> portSelf_       ref p
        OutPortId  All           -> portIOExpanded_ ref p 0 False
        InPortId  (Arg        i) -> portIOExpanded_ ref p i True
        OutPortId (Projection i) -> portIOExpanded_ ref p i False

port_ :: Ref App -> Port -> Int -> Bool -> ReactElementM ViewEventHandler ()
port_ ref p numOfPorts isOnly =
    React.viewWithSKey port (jsShow $ p ^. Port.portId) (ref, numOfPorts, isOnly, p) mempty where

portExpanded_ :: Ref App -> Port -> ReactElementM ViewEventHandler ()
portExpanded_ ref p =
    React.viewWithSKey portExpanded (jsShow $ p ^. Port.portId) (ref, p) mempty

portSelf_ :: Ref App -> Port -> ReactElementM ViewEventHandler ()
portSelf_ ref port = do
    let portRef = port ^. Port.portRef
        color   = toJSString $ port ^. Port.color
        portId  = port ^. Port.portId
    g_
        [ "className" $= "port port--self" ] $ do
        circle_
            [ "className" $= "port__shape"
            , "key"       $= (fromString (show portId ) <> "a")
            , "fill"      $= color
            ] mempty
        circle_
            [ onMouseDown $ handleMouseDown ref portRef
            , onMouseUp   $ handleMouseUp   ref portRef
            , "className" $= "port__select"
            , "key"       $= (fromString (show portId ) <> "b")
            ] mempty

portSingle_ :: Ref App -> Port -> ReactElementM ViewEventHandler ()
portSingle_ ref port = do
    let portRef = port ^. Port.portRef
        portId  = port ^. Port.portId
        color   = toJSString $ port ^. Port.color
        r1 = show2 . (+) nodeRadius
        r2 = show2 . (-) nodeRadius'
        svgPath a b c = fromString $ "M0 -" <> r1 a <> " A " <> r1 a <> " " <> r1 a <> " 0 0 " <> show b <> " 0 "  <> r1 a <>
                                    " L0 "  <> r2 a <> " A " <> r2 a <> " " <> r2 a <> " 1 0 " <> show c <> " 0 -" <> r2 a <> " Z "
    g_ [ "className" $= "port port--o--single" ] $ do
        path_
            [ "className" $= "port__shape"
            , "key"       $= (fromString (show portId) <> "a" )
            , "fill"      $= color
            , "d"         $= (svgPath 0 0 1 <> svgPath 0 1 0)
            ] mempty
        path_
            [ onMouseDown $ handleMouseDown ref portRef
            , onMouseUp   $ handleMouseUp   ref portRef
            , "className" $= "port__select"
            , "key"       $= (fromString (show portId ) <> "b")
            , "d"         $= (svgPath 3 0 1 <> svgPath 3 1 0)
            ] mempty

portIO_ :: Ref App -> Port -> Int -> Int -> Bool -> ReactElementM ViewEventHandler ()
portIO_ ref port num numOfPorts isInput = do
    let portRef = port ^. Port.portRef
        portId  = port ^. Port.portId
        color   = toJSString $ port ^. Port.color
        classes  = if isInput then "port port--i port--i--" else "port port--o port--o--"
        svgFlag1 = if isInput then "1"  else "0"
        svgFlag2 = if isInput then "0"  else "1"
        mod      = if isInput then -1.0 else 1.0
        startPortArcX r = r * sin(portAngleStart num numOfPorts r * mod)
        startPortArcY r = r * cos(portAngleStart num numOfPorts r * mod)
        stopPortArcX  r = r * sin(portAngleStop  num numOfPorts r * mod)
        stopPortArcY  r = r * cos(portAngleStop  num numOfPorts r * mod)
        ax = show2 . startPortArcX . (+) nodeRadius
        ay = show2 . startPortArcY . (+) nodeRadius
        bx = show2 . stopPortArcX  . (+) nodeRadius
        by = show2 . stopPortArcY  . (+) nodeRadius
        cx = show2 . stopPortArcX  . (-) nodeRadius'
        cy = show2 . stopPortArcY  . (-) nodeRadius'
        dx = show2 . startPortArcX . (-) nodeRadius'
        dy = show2 . startPortArcY . (-) nodeRadius'
        r1 = show2 . (+) nodeRadius
        r2 = show2 . (-) nodeRadius'
        svgPath a = fromString $ "M"  <> ax a <> " " <> ay a <>
                                " A " <> r1 a <> " " <> r1 a <> " 1 0 " <> svgFlag1 <> " " <> bx a <> " " <> by a <>
                                " L " <> cx a <> " " <> cy a <>
                                " A " <> r2 a <> " " <> r2 a <> " 1 0 " <> svgFlag2 <> " " <> dx a <> " " <> dy a <>
                                " Z"
    g_
        [ "className" $= (fromString $ classes <> show (num+1)) ] $ do
        path_
            [ "className" $= "port__shape"
            , "key"       $= (fromString (show portId) <> "a")
            , "fill"      $= color
            , "d"         $= svgPath 0
            ] mempty
        path_
            [ onMouseDown $ handleMouseDown ref portRef
            , onMouseUp   $ handleMouseUp   ref portRef
            , "className" $= "port__select"
            , "key"       $= (fromString (show portId) <> "b")
            , "d"         $= svgPath 3
            ] mempty

portIOExpanded_ :: Ref App -> Port -> Int -> Bool -> ReactElementM ViewEventHandler ()
portIOExpanded_ ref port num isInput = do
    let portRef = port ^. Port.portRef
        portId  = port ^. Port.portId
        color   = toJSString $ port ^. Port.color
        classes = if isInput then "port port--i port--i--" else "port port--o port--o--"
        n       = if isInput then 1 else 0
        r       = show2 . (+3)
    g_
        [ "className" $= fromString (classes <> show (num + 1)) ] $ do
        circle_
            [ "className" $= "port__shape"
            , "key"       $= fromString (show portId <> show num <> "a")
            , "fill"      $= color
            , "r"         $= fromString (r 0)
            , "cy"        $= fromString (show2 $ lineHeight * fromIntegral (num + n) )
            ] mempty
        circle_
            [ onMouseDown $ handleMouseDown ref portRef
            , onMouseUp   $ handleMouseUp   ref portRef
            , "className" $= "port__select"
            , "key"       $= fromString (show portId <> show num <> "b")
            , "r"         $= fromString (r 3)
            , "cy"        $= fromString (show2 $ lineHeight * fromIntegral (num + n) )
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
