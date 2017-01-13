{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.PortControl
    ( portControl_
    ) where

import           React.Flux                   as React

import qualified Empire.API.Data.DefaultValue as DefaultValue
import           Empire.API.Data.Port         (InPort (..), InPort (..), OutPort (..), PortId (..))
import qualified Empire.API.Data.Port         as PortAPI
import           Empire.API.Data.PortRef      (AnyPortRef (..), toAnyPortRef)
import qualified Empire.API.Data.ValueType    as ValueType
import qualified Event.UI                     as UI
import           Luna.Studio.Prelude          hiding (group)
import qualified Luna.Studio.React.Event.Node as Node
import           Luna.Studio.React.Model.App (App)
import           Luna.Studio.React.Model.Node (NodeId)
import           Luna.Studio.React.Model.Port (Port)
import qualified Luna.Studio.React.Model.Port as Port
import           Luna.Studio.React.Store      (Ref, dispatch)
import qualified Luna.Studio.State.Slider     as Slider



portControl_ :: Ref App -> NodeId -> Bool -> Port -> ReactElementM ViewEventHandler ()
portControl_ ref nodeId isLiteral port = React.viewWithSKey portControl "portControl" (ref, nodeId, isLiteral, port) mempty

portControl :: ReactView (Ref App, NodeId, Bool, Port)
portControl = React.defineView "portControl" $ \(ref, nodeId, isLiteral, port) ->
    let portRef = toAnyPortRef nodeId $ port ^. Port.portId
    in case port ^. Port.portId of
        InPortId  (Arg _) -> inPortControl_ ref portRef port
        OutPortId All     -> when isLiteral $ inPortControl_ ref portRef port
        _                 -> return ()

inPortControl_ :: Ref App -> AnyPortRef -> Port -> ReactElementM ViewEventHandler ()
inPortControl_ ref portRef port = React.viewWithSKey inPortControl "inPortControl" (ref, portRef, port) mempty

inPortControl :: ReactView (Ref App, AnyPortRef, Port)
inPortControl = React.defineView "inPortControl" $ \(ref, portRef, port) ->
    div_
        [ "key" $= fromString (show $ port ^. Port.portId)
        , "className" $= "row row--arg"
        ] $ do
        div_
            [ "key"       $= "label"
            , "className" $= "label"
            ] $ elemString $ fromString $ port ^. Port.name
        div_
            [ "key"       $= "value"
            , "className" $= "value"
            ] $
            case port ^. Port.state of
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
                        --TODO[react]: +1 with Q and up key, -1 with W and down key, edit on double click
                        , onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.PortInitSlider m portRef $ Slider.Discrete value)
                        ] $
                        elemString $ fromString $ show value
                ValueType.ContinuousNumber -> do
                    let value = fromMaybe 0.0 $ defVal ^? DefaultValue._Constant . DefaultValue._DoubleValue
                    div_
                        [ "className" $= "horizontal-slider"
                        --TODO[react]: +1 with Q and up key, -1 with W and down key, edit on double click
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
                        , onKeyDown   $ \e k -> let val = target e "value" in stopPropagation e : dispatch ref (UI.NodeEvent $ Node.PortApplyString k portRef $ defaultValue val)
                        , onChange    $ \e   -> let val = target e "value" in dispatch ref $ UI.NodeEvent $ Node.PortEditString portRef $ defaultValue val
                        ]
                ValueType.Bool -> do
                    let value = fromMaybe True $ defVal ^? DefaultValue._Constant . DefaultValue._BoolValue
                        defaultValue = DefaultValue.Constant $ DefaultValue.BoolValue $ not value
                    button_
                        [ onClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.PortSetDefaultValue portRef $ defaultValue
                        ] $
                        elemString $ fromString $ show value
                ValueType.Other ->
                    elemString $ fromString $ "(other)"
