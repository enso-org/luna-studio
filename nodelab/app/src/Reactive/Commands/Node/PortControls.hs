{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Node.PortControls
    ( makePortControl
    ) where

import           Luna.Studio.Prelude               hiding (group)
import           Luna.Studio.Data.Vector

import qualified Data.Map.Lazy                   as Map
import qualified Data.Text.Lazy                  as Text

import           Object.UITypes                  (WidgetId)
import qualified Object.Widget.Button            as Button
import qualified Object.Widget.Group             as Group
import qualified Object.Widget.Label             as Label
import qualified Object.Widget.LabeledTextBox    as LabeledTextBox
import qualified Object.Widget.Number.Continuous as ContinuousNumber
import qualified Object.Widget.Number.Discrete   as DiscreteNumber
import qualified Object.Widget.Toggle            as Toggle
import qualified UI.Handlers.Button              as Button
import           UI.Handlers.Generic             (onValueChanged)
import           UI.Instances ()

import           Reactive.Commands.Command       (Command)
import qualified Reactive.Commands.UIRegistry    as UICmd
import           Luna.Studio.State.UIRegistry       (addHandler)
import qualified Luna.Studio.State.UIRegistry       as UIRegistry

import qualified Reactive.Commands.Batch         as BatchCmd

import           UI.Layout                       as Layout

import qualified Style.Node                      as Style
import qualified Style.Types                     as Style

import qualified Empire.API.Data.DefaultValue    as DefaultValue
import           Empire.API.Data.Node            (Node)
import qualified Empire.API.Data.Node            as Node
import           Empire.API.Data.Port            (InPort (..), InPort (..), OutPort (..), Port (..), PortId (..))
import qualified Empire.API.Data.Port            as Port
import           Empire.API.Data.PortRef         (AnyPortRef (..), toAnyPortRef)
import qualified Empire.API.Data.ValueType       as ValueType

isLiteral :: Getter Node Bool
isLiteral = to $ isLiteral' where
    isLiteral' node = not $ any isIn' portIds where
        portIds = Map.keys $ node ^. Node.ports
        isIn' :: PortId -> Bool
        isIn' (OutPortId _) = False
        isIn' (InPortId  _) = True


makePortControl :: WidgetId -> Node  -> Port -> Command UIRegistry.State ()
makePortControl groupParent node port =
    let portRef = toAnyPortRef nodeId $ port ^. Port.portId
        nodeId  = node ^. Node.nodeId
    in
    case port ^. Port.portId of
        InPortId  (Arg _) -> makeInPortControl groupParent portRef port
        OutPortId All     -> when (node ^. isLiteral) $ makeInPortControl groupParent portRef port
        _ -> return ()

makeInPortControl :: WidgetId -> AnyPortRef -> Port -> Command UIRegistry.State ()
makeInPortControl parent portRef port = case port ^. Port.state of
    Port.NotConnected    -> do
        case port ^. Port.valueType . ValueType.toEnum of
            ValueType.Other -> return ()
            _               -> do
                let group = Group.create & Group.style . Group.padding .~ Style.Padding 0.0 0.0 0.0 Style.setLabelOffsetX
                groupId <- UICmd.register parent group (Layout.horizontalLayoutHandler 0.0)
                let label  = Label.create Style.setLabelSize (Text.pack $ port ^. Port.name)
                           & Label.position . x .~ Style.setLabelOffsetX
                    button = Button.create Style.setButtonSize "not set"
                    zeroValue = case port ^. Port.valueType . ValueType.toEnum of
                        ValueType.DiscreteNumber   -> DefaultValue.IntValue    def
                        ValueType.ContinuousNumber -> DefaultValue.DoubleValue def
                        ValueType.String           -> DefaultValue.StringValue def
                        ValueType.Bool             -> DefaultValue.BoolValue   False
                        _                          -> undefined
                    handlers = addHandler (Button.ClickedHandler $ \_ -> BatchCmd.setDefaultValue portRef (DefaultValue.Constant $ zeroValue)
                        ) mempty

                UICmd.register_ groupId label def
                UICmd.register_ groupId button handlers
    Port.Connected       -> do
        let widget = Label.create (Style.portControlSize & x -~ Style.setLabelOffsetX) (Text.pack $ (port ^. Port.name) <> " (connected)")
                   & Label.position . x .~ Style.setLabelOffsetX
        void $ UICmd.register parent widget def
    Port.WithDefault defVal -> void $ case port ^. Port.valueType . ValueType.toEnum of
        ValueType.DiscreteNumber -> do
            let label = port ^. Port.name
                value = fromMaybe 0 $ defVal ^? DefaultValue._Constant . DefaultValue._IntValue
                widget = DiscreteNumber.create Style.portControlSize (Text.pack $ label) value
                handlers = onValueChanged $ \val _ -> BatchCmd.setDefaultValue portRef (DefaultValue.Constant $ DefaultValue.IntValue val)
            UICmd.register parent widget handlers
        ValueType.ContinuousNumber -> do
            let label = port ^. Port.name
                value = fromMaybe 0.0 $ defVal ^? DefaultValue._Constant . DefaultValue._DoubleValue
                widget = ContinuousNumber.create Style.portControlSize (Text.pack $ label) value
                handlers = onValueChanged $ \val _ -> BatchCmd.setDefaultValue portRef (DefaultValue.Constant $ DefaultValue.DoubleValue val)
            UICmd.register parent widget handlers
        ValueType.String -> do
            let label = port ^. Port.name
                value = fromMaybe "" $ defVal ^? DefaultValue._Constant . DefaultValue._StringValue
                widget = LabeledTextBox.create Style.portControlSize (Text.pack $ label) (Text.pack $ value)
                handlers = onValueChanged $ \val _ -> BatchCmd.setDefaultValue portRef (DefaultValue.Constant $ DefaultValue.StringValue $ Text.unpack val)
            UICmd.register parent widget handlers
        ValueType.Bool -> do
            let label = port ^. Port.name
                value = fromMaybe True $ defVal ^? DefaultValue._Constant . DefaultValue._BoolValue
                widget = Toggle.create Style.portControlSize (Text.pack $ label) value
                handlers = onValueChanged $ \val _ -> BatchCmd.setDefaultValue portRef (DefaultValue.Constant $ DefaultValue.BoolValue val)
            UICmd.register parent widget handlers
        ValueType.Other -> do
            let widget = Label.create (Style.portControlSize & x -~ Style.setLabelOffsetX) (Text.pack $ (port ^. Port.name) )
                       & Label.position . x .~ Style.setLabelOffsetX

            UICmd.register parent widget mempty
