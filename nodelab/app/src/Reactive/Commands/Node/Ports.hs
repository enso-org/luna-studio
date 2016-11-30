{-# LANGUAGE OverloadedStrings #-}

module Reactive.Commands.Node.Ports
    ( displayPorts
    ) where

import           Control.Monad.State                 hiding (State)
import qualified Data.Map.Lazy                       as Map
import qualified Data.Text.Lazy                      as Text
import           Utils.PreludePlus
import           Utils.Vector

import           Object.Widget                       (WidgetId)
import qualified Object.Widget.Label                 as Label
import qualified Object.Widget.Node                  as Model
import qualified Object.Widget.Port                  as PortModel
import qualified UI.Handlers.Node                    as Node

import           Reactive.Commands.Command           (Command)
import           Reactive.Commands.Node.PortControls (makePortControl)
import           Reactive.Commands.Node.Ports.Colors (colorPort)
import qualified Reactive.Commands.UIRegistry        as UICmd
import           Reactive.State.Global               (inRegistry)
import qualified Reactive.State.Global               as Global
import qualified Reactive.State.Graph                as Graph
import qualified Reactive.State.UIRegistry           as UIRegistry

import           Empire.API.Data.Node                (Node)
import qualified Empire.API.Data.Node                as Node
import           Empire.API.Data.Port                (InPort (..), OutPort (..), Port (..), PortId (..))
import qualified Empire.API.Data.Port                as Port
import           Empire.API.Data.PortRef             (toAnyPortRef)
import qualified Empire.API.Data.ValueType           as ValueType

makePorts :: Node -> [PortModel.Port]
makePorts node = makePort <$> ports where
    nodeId  = node ^. Node.nodeId
    makePort port = PortModel.Port portRef angle (portCount portId) isOnly (colorPort port) False where
        portId  = port ^. Port.portId
        portRef = toAnyPortRef nodeId portId
        angle   = PortModel.defaultAngle (portCount portId) portId
    ports   = Map.elems $ node ^. Node.ports
    portIds = Map.keys  $ node ^. Node.ports
    portCount :: PortId -> Int
    portCount (OutPortId _) = sum $ fmap isOut portIds where
        isOut :: PortId -> Int
        isOut (OutPortId _) = 1
        isOut (InPortId  _) = 0
    portCount (InPortId  _) = sum $ fmap isIn  portIds where
        isIn :: PortId -> Int
        isIn (OutPortId _)      = 0
        isIn (InPortId (Arg _)) = 1
        isIn (InPortId Self)    = 0
    isOnly :: Bool
    isOnly = 0 == (sum $ fmap shouldCount portIds) where
        shouldCount :: PortId -> Int
        shouldCount (OutPortId All)            = 0
        shouldCount (InPortId Self)            = 0
        shouldCount (OutPortId (Projection _)) = 1
        shouldCount (InPortId (Arg _))         = 1


displayPorts :: WidgetId -> Node -> Command Global.State ()
displayPorts wid node = do
        portGroup <- inRegistry $ UICmd.get wid $ Model.elements . Model.portGroup
        oldPorts  <- inRegistry $ UICmd.children portGroup
        oldPortWidgets <- forM oldPorts $ \wid' -> inRegistry $ (UICmd.lookup wid')
        let portRefs = (view PortModel.portRef) <$> oldPortWidgets
        forM_ portRefs $ \wid' -> Global.graph . Graph.portWidgetsMap . at wid' .= Nothing
        inRegistry $ mapM_ UICmd.removeWidget oldPorts

        groupId      <- inRegistry $ Node.portControlsGroupId wid
        portControls <- inRegistry $ UICmd.children groupId
        inRegistry $ mapM_ UICmd.removeWidget portControls

        inLabelsGroupId <- inRegistry $ Node.inLabelsGroupId wid
        inLabels        <- inRegistry $ UICmd.children inLabelsGroupId
        inRegistry $ mapM_ UICmd.removeWidget inLabels

        outLabelsGroupId <- inRegistry $ Node.outLabelsGroupId wid
        outLabels        <- inRegistry $ UICmd.children outLabelsGroupId
        inRegistry $ mapM_ UICmd.removeWidget outLabels

        forM_ (makePorts node    ) $ \p -> do
            portWidgetId <- inRegistry $ UICmd.register portGroup p def
            Global.graph . Graph.portWidgetsMap . at (p ^. PortModel.portRef) ?= portWidgetId
        inRegistry $ forM_ (node ^. Node.ports) $ makePortControl groupId node
        inRegistry $ forM_ (node ^. Node.ports) $ \p -> case p ^. Port.portId of
            InPortId  Self -> return ()
            InPortId  _    -> makePortLabel inLabelsGroupId  p
            OutPortId _    -> makePortLabel outLabelsGroupId p


makePortLabel :: WidgetId -> Port -> Command UIRegistry.State ()
makePortLabel parent port = do
    let align = case port ^. Port.portId of
            InPortId  _ -> Label.Right
            OutPortId _ -> Label.Left
        label = Label.create (Vector2 360 15) text & Label.alignment .~ align
        text  = (Text.pack $ port ^. Port.name) <> " :: " <> portType
        portType = port ^. Port.valueType . ValueType.toText
    UICmd.register_ parent label def
