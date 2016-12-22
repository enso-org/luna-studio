{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.PortControl
    ( portControl_
    ) where


import qualified Data.Map.Lazy                   as Map
import qualified Data.Text.Lazy                  as Text
import           React.Flux

import qualified Empire.API.Data.DefaultValue    as DefaultValue
import qualified Empire.API.Data.Node            as NodeAPI
import           Empire.API.Data.Port            (InPort (..), InPort (..), OutPort (..), PortId (..))
import qualified Empire.API.Data.Port            as PortAPI
import           Empire.API.Data.PortRef         (AnyPortRef (..), portId', toAnyPortRef)
import qualified Empire.API.Data.ValueType       as ValueType
import qualified Event.UI                        as UI
import qualified Luna.Studio.Commands.Batch      as BatchCmd
import           Luna.Studio.Commands.Command    (Command)
import qualified Luna.Studio.Commands.UIRegistry as UICmd
import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude             hiding (group)
import qualified Luna.Studio.React.Event.Node    as Node
import           Luna.Studio.React.Store         (Ref, dispatch)
import           Luna.Studio.State.UIRegistry    (addHandler)
import qualified Luna.Studio.State.UIRegistry    as UIRegistry
import           Object.UITypes                  (WidgetId)
import qualified Object.Widget.Button            as Button
import qualified Object.Widget.Group             as Group
import qualified Object.Widget.Label             as Label
import qualified Object.Widget.LabeledTextBox    as LabeledTextBox
import           Object.Widget.Node              (Node)
import qualified Object.Widget.Node              as Node
import qualified Object.Widget.Number.Continuous as ContinuousNumber
import qualified Object.Widget.Number.Discrete   as DiscreteNumber
import           Object.Widget.Port              (Port)
import qualified Object.Widget.Port              as Port
import qualified Object.Widget.Toggle            as Toggle
import qualified Style.Node                      as Style
import qualified Style.Types                     as Style
import qualified UI.Handlers.Button              as Button
import           UI.Handlers.Generic             (onValueChanged)
import           UI.Instances                    ()
import           UI.Layout                       as Layout



isLiteral :: Getter Node Bool
isLiteral = to $ isLiteral' where
    isLiteral' node = not $ any isIn' portIds where
        portIds = map portId' $ Map.keys $ node ^. Node.ports
        isIn' :: PortId -> Bool
        isIn' (OutPortId _) = False
        isIn' (InPortId  _) = True


portControl_ :: Ref Node -> Node -> Port -> ReactElementM ViewEventHandler ()
portControl_ ref node port =
    let portRef = toAnyPortRef nodeId $ port ^. Port.portId
        nodeId  = node ^. Node.nodeId
    in
    case port ^. Port.portId of
        InPortId  (Arg _) -> inPortControl_ ref portRef port
        OutPortId All     -> when (node ^. isLiteral) $ inPortControl_ ref portRef port
        _ -> return ()

inPortControl_ :: Ref Node -> AnyPortRef -> Port -> ReactElementM ViewEventHandler ()
inPortControl_ ref portRef port = case port ^. Port.state of
    PortAPI.NotConnected    -> do
        case port ^. Port.valueType . ValueType.toEnum of
            ValueType.Other -> return ()
            _               -> do
                let zeroValue = case port ^. Port.valueType . ValueType.toEnum of
                        ValueType.DiscreteNumber   -> DefaultValue.IntValue    def
                        ValueType.ContinuousNumber -> DefaultValue.DoubleValue def
                        ValueType.String           -> DefaultValue.StringValue def
                        ValueType.Bool             -> DefaultValue.BoolValue   False
                        _                          -> undefined
                    defaultValue = DefaultValue.Constant zeroValue
                div_ [ "className" $= "label" ] $ elemString $ fromString $ port ^. Port.name
                div_ [ "className" $= "value" ] $
                    button_
                        [ onClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.SetDefaultValue portRef defaultValue] $
                        elemString "not set"
                return ()
    PortAPI.Connected       -> do
        let widget = Label.create (Style.portControlSize & x -~ Style.setLabelOffsetX) (Text.pack $ (port ^. Port.name) <> " (connected)")
                   & Label.position . x .~ Style.setLabelOffsetX
        -- void $ UICmd.register widget def
        return ()
    PortAPI.WithDefault defVal -> void $ case port ^. Port.valueType . ValueType.toEnum of
        ValueType.DiscreteNumber -> do
            let label = port ^. Port.name
                value = fromMaybe 0 $ defVal ^? DefaultValue._Constant . DefaultValue._IntValue
                widget = DiscreteNumber.create Style.portControlSize (Text.pack $ label) value
                handlers = onValueChanged $ \val _ -> BatchCmd.setDefaultValue portRef (DefaultValue.Constant $ DefaultValue.IntValue val)
            -- UICmd.register widget handlers
            return ()
        ValueType.ContinuousNumber -> do
            let label = port ^. Port.name
                value = fromMaybe 0.0 $ defVal ^? DefaultValue._Constant . DefaultValue._DoubleValue
                widget = ContinuousNumber.create Style.portControlSize (Text.pack $ label) value
                handlers = onValueChanged $ \val _ -> BatchCmd.setDefaultValue portRef (DefaultValue.Constant $ DefaultValue.DoubleValue val)
            -- UICmd.register widget handlers
            return ()
        ValueType.String -> do
            let label = port ^. Port.name
                value = fromMaybe "" $ defVal ^? DefaultValue._Constant . DefaultValue._StringValue
                widget = LabeledTextBox.create Style.portControlSize (Text.pack $ label) (Text.pack $ value)
                handlers = onValueChanged $ \val _ -> BatchCmd.setDefaultValue portRef (DefaultValue.Constant $ DefaultValue.StringValue $ Text.unpack val)
            -- UICmd.register widget handlers
            return ()
        ValueType.Bool -> do
            let label = port ^. Port.name
                value = fromMaybe True $ defVal ^? DefaultValue._Constant . DefaultValue._BoolValue
                widget = Toggle.create Style.portControlSize (Text.pack $ label) value
                handlers = onValueChanged $ \val _ -> BatchCmd.setDefaultValue portRef (DefaultValue.Constant $ DefaultValue.BoolValue val)
            -- UICmd.register widget handlers
            return ()
        ValueType.Other -> do
            let widget = Label.create (Style.portControlSize & x -~ Style.setLabelOffsetX) (Text.pack $ (port ^. Port.name) )
                       & Label.position . x .~ Style.setLabelOffsetX

            -- UICmd.register widget mempty
            return ()
