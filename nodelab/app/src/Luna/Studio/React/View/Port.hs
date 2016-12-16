{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Port where

import           Luna.Studio.Prelude

import           Empire.API.Data.Node               (NodeId)
import           Empire.API.Data.Port               (InPort (..), OutPort (..), PortId (..))
import qualified Event.UI                           as UI
import           Luna.Studio.Data.Color             (Color (Color))
import           Luna.Studio.Data.HSL               (color')
import qualified Luna.Studio.React.Event.Node       as Node
import           Luna.Studio.React.Model.Node       (Node)
import           Luna.Studio.React.Store            (Ref, dispatch)
import           Luna.Studio.React.View.Connection  (connectionWidth)
import qualified Numeric                            as Numeric
import           Object.Widget.Port                 (Port (..))
import           React.Flux                         hiding (view)
import qualified React.Flux                         as React


showF :: Double -> String
showF a = Numeric.showFFloat (Just 1) a ""

nodeRadius :: Double
nodeRadius = 20

nodeRadius' :: Double
nodeRadius' = nodeRadius - connectionWidth

portRadius :: Double
portRadius = nodeRadius - connectionWidth/2

portGap :: Double -> Double
portGap r = 0.15 * nodeRadius / r -- to avoid gap narrowing


portAngle :: Int -> Double
portAngle numOfPorts = pi / fromIntegral numOfPorts

portAngleStart :: Int -> Int -> Double -> Double
portAngleStart num numOfPorts r =
    let number = fromIntegral num + 1
        gap    = portGap r
        t      = portAngle numOfPorts
    in  number * t - pi - t + gap/2

portAngleStop :: Int -> Int -> Double -> Double
portAngleStop num numOfPorts r =
    let number = fromIntegral num + 1
        gap    = portGap r
        t      = portAngle numOfPorts
    in  number * t - pi - gap/2


name :: JSString
name = "port"

port :: Ref Node -> NodeId -> Int -> Bool -> ReactView Port
port nodeRef nodeId numOfPorts isOnly = React.defineView name $ \p -> do
    drawPort_ nodeRef nodeId p numOfPorts isOnly

port_ :: Ref Node -> NodeId -> Port -> Int -> Bool -> ReactElementM ViewEventHandler ()
port_ ref nodeId p numOfPorts isOnly = React.view (port ref nodeId numOfPorts isOnly) p mempty

drawPort_ :: Ref Node -> NodeId -> Port -> Int -> Bool -> ReactElementM ViewEventHandler ()
drawPort_ nodeRef nodeId (Port _ portId@(InPortId   Self         ) _ _) _          _     = drawPortSelf_   nodeRef nodeId portId
drawPort_ nodeRef nodeId (Port _ portId@(OutPortId  All          ) _ _) _          True  = drawPortSingle_ nodeRef nodeId portId
drawPort_ nodeRef nodeId (Port _ portId@(OutPortId  All          ) _ _) numOfPorts False = drawPortIO_     nodeRef nodeId portId 0 numOfPorts (-1) "1" "0"
drawPort_ nodeRef nodeId (Port _ portId@(InPortId  (Arg        i)) _ _) numOfPorts _     = drawPortIO_     nodeRef nodeId portId i numOfPorts   1  "0" "1"
drawPort_ nodeRef nodeId (Port _ portId@(OutPortId (Projection i)) _ _) numOfPorts _     = drawPortIO_     nodeRef nodeId portId i numOfPorts (-1) "1" "0"

drawPortSelf_ :: Ref Node -> NodeId -> PortId -> ReactElementM ViewEventHandler ()
drawPortSelf_ nodeRef nodeId portId = let color = color' $ Color 5 in
    circle_
        [ onMouseDown $ \e _ -> stopPropagation e : dispatch nodeRef (UI.NodeEvent $ Node.StartConnection nodeId portId)
        , onMouseUp   $ \e _ -> stopPropagation e : dispatch nodeRef (UI.NodeEvent $ Node.EndConnection   nodeId portId)
        , "className" $= "port port--self"
        , "fill"      $= color
        , "stroke"    $= color
        ] mempty

drawPortSingle_ :: Ref Node -> NodeId -> PortId -> ReactElementM ViewEventHandler ()
drawPortSingle_ nodeRef nodeId portId = let color = color' $ Color 5 in
    circle_
        [ onMouseDown $ \e _ -> stopPropagation e : dispatch nodeRef (UI.NodeEvent $ Node.StartConnection nodeId portId)
        , onMouseUp   $ \e _ -> stopPropagation e : dispatch nodeRef (UI.NodeEvent $ Node.EndConnection   nodeId portId)
        , "className" $= "port port--o port--o--single"
        , "stroke"    $= color
        ] mempty


drawPortIO_ :: Ref Node -> NodeId -> PortId -> Int -> Int -> Double -> String -> String -> ReactElementM ViewEventHandler ()
drawPortIO_ nodeRef nodeId portId num numOfPorts mod1 mod2 mod3 = do

    let color   = color' $ Color 5 --TODO [Piotr Młodawski]: get color from model

        t1  = portAngleStart num numOfPorts nodeRadius
        t2  = portAngleStop  num numOfPorts nodeRadius

        t1' = portAngleStart num numOfPorts nodeRadius'
        t2' = portAngleStop  num numOfPorts nodeRadius'

        ax = showF $ nodeRadius * sin(t1 * mod1)
        ay = showF $ nodeRadius * cos(t1 * mod1)

        bx = showF $ nodeRadius * sin(t2 * mod1)
        by = showF $ nodeRadius * cos(t2 * mod1)

        cx = showF $ nodeRadius' * sin(t2' * mod1)
        cy = showF $ nodeRadius' * cos(t2' * mod1)

        dx = showF $ nodeRadius' * sin(t1' * mod1)
        dy = showF $ nodeRadius' * cos(t1' * mod1)

        svgPath = fromString $ "M" <> ax <> " " <> ay <> " A " <> show nodeRadius  <> " " <> show nodeRadius  <> " 1 0 " <> mod2 <> " " <> bx <> " " <> by <>
                              " L" <> cx <> " " <> cy <> " A " <> show nodeRadius' <> " " <> show nodeRadius' <> " 1 0 " <> mod3 <> " " <> dx <> " " <> dy <>
                              " L" <> ax <> " " <> ay

    path_
        [ onMouseDown $ \e _ -> stopPropagation e : dispatch nodeRef (UI.NodeEvent $ Node.StartConnection nodeId portId)
        , onMouseUp   $ \e _ -> stopPropagation e : dispatch nodeRef (UI.NodeEvent $ Node.EndConnection   nodeId portId)
        , "className" $= (fromString $ "port port--i port--i--" <> show (num+1))
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
