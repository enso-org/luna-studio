{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.PortControl
    ( portControl_
    ) where

import qualified Data.Map.Lazy                as Map
import           React.Flux

import qualified Empire.API.Data.DefaultValue as DefaultValue
import           Empire.API.Data.Port         (InPort (..), InPort (..), OutPort (..), PortId (..))
import qualified Empire.API.Data.Port         as PortAPI
import           Empire.API.Data.PortRef      (AnyPortRef (..), portId', toAnyPortRef)
import qualified Empire.API.Data.ValueType    as ValueType
import qualified Event.UI                     as UI
import           Luna.Studio.Prelude          hiding (group)
import qualified Luna.Studio.React.Event.Node as Node
import           Luna.Studio.React.Store      (Ref, dispatch)
import qualified Luna.Studio.State.Slider     as Slider
import           Object.Widget.Node           (Node)
import qualified Object.Widget.Node           as Node
import           Object.Widget.Port           (Port)
import qualified Object.Widget.Port           as Port
import           UI.Instances                 ()



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
inPortControl_ ref portRef port = do
    div_ [ "className" $= "label" ] $ elemString $ fromString $ port ^. Port.name
    div_ [ "className" $= "value" ] $ case port ^. Port.state of
        PortAPI.NotConnected    -> do
            case port ^. Port.valueType . ValueType.toEnum of
                ValueType.Other -> elemString $ fromString $ "(other)"
                _               -> do
                    let zeroValue = case port ^. Port.valueType . ValueType.toEnum of
                            ValueType.DiscreteNumber   -> DefaultValue.IntValue    def
                            ValueType.ContinuousNumber -> DefaultValue.DoubleValue def
                            ValueType.String           -> DefaultValue.StringValue def
                            ValueType.Bool             -> DefaultValue.BoolValue   False
                            _                          -> undefined
                        defaultValue = DefaultValue.Constant zeroValue
                    button_
                        [ onClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.PortSetDefaultValue portRef defaultValue ] $
                        elemString "not set"
        PortAPI.Connected       -> do
            elemString $ fromString $ "(connected)"
        PortAPI.WithDefault defVal -> void $ case port ^. Port.valueType . ValueType.toEnum of
            ValueType.DiscreteNumber -> do
                let value = fromMaybe 0 $ defVal ^? DefaultValue._Constant . DefaultValue._IntValue
                div_
                    [ "className" $= "horizontal-slider"
                    , onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.PortInitSlider m portRef $ Slider.Discrete value)
                    ] $
                    elemString $ fromString $ show value
            ValueType.ContinuousNumber -> do
                let value = fromMaybe 0.0 $ defVal ^? DefaultValue._Constant . DefaultValue._DoubleValue
                div_
                    [ "className" $= "horizontal-slider"
                    , onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.PortInitSlider m portRef $ Slider.Continous value)
                    ] $
                    elemString $ fromString $ show value
            ValueType.String -> do
                let value = fromMaybe "" $ defVal ^? DefaultValue._Constant . DefaultValue._StringValue
                    defaultValue val = DefaultValue.Constant $ DefaultValue.StringValue val
                input_
                    ["id" $= "focus-portcontrol"
                    ,"value" $= fromString value
                    , onMouseDown $ \e _ -> [stopPropagation e]
                    , onKeyDown   $ \e k ->  [stopPropagation e]
                    , onChange    $ \e -> let val = target e "value" in dispatch ref $ UI.NodeEvent $ Node.PortSetDefaultValue portRef $ defaultValue val
                    ]
            ValueType.Bool -> do
                let value = fromMaybe True $ defVal ^? DefaultValue._Constant . DefaultValue._BoolValue
                    defaultValue = DefaultValue.Constant $ DefaultValue.BoolValue $ not value
                button_
                    [ onClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.PortSetDefaultValue portRef $ defaultValue
                    ] $
                    elemString $ fromString $ show value
            ValueType.Other ->
                elemString $ fromString $ " "
