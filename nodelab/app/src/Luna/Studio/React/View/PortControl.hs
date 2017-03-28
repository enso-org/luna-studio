{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.PortControl
    ( portControl_
    ) where

import           Empire.API.Data.Port         (InPort (Arg), OutPort (All), PortId (InPortId, OutPortId))
import qualified Empire.API.Data.Port         as PortAPI
import qualified Empire.API.Data.PortDefault  as PortDefault
import qualified Empire.API.Data.ValueType    as ValueType
import qualified JS.Config                    as Config
import           Luna.Studio.Data.PortRef     (AnyPortRef (..), toAnyPortRef)
import qualified Luna.Studio.Event.UI         as UI
import           Luna.Studio.Prelude          hiding (group)
import qualified Luna.Studio.React.Event.Node as Node
import           Luna.Studio.React.Model.App  (App)
import           Luna.Studio.React.Model.Node (NodeLoc)
import           Luna.Studio.React.Model.Port (Port)
import qualified Luna.Studio.React.Model.Port as Port
import           Luna.Studio.React.Store      (Ref, dispatch)
import qualified Luna.Studio.React.View.Style as Style
import           Luna.Studio.State.Action     (InitValue (Continous, Discrete))
import           React.Flux                   as React




portControl_ :: Ref App -> NodeLoc -> Bool -> Port -> ReactElementM ViewEventHandler ()
portControl_ ref nl isLiteral port = React.viewWithSKey portControl (jsShow $ port ^. Port.portId) (ref, nl, isLiteral, port) mempty

portControl :: ReactView (Ref App, NodeLoc, Bool, Port)
portControl = React.defineView "portControl" $ \(ref, nl, isLiteral, port) ->
    let portRef = toAnyPortRef nl $ port ^. Port.portId
    in case port ^. Port.portId of
        InPortId  (Arg _) -> inPortControl_ ref portRef port
        OutPortId All     -> when isLiteral $ inPortControl_ ref portRef port
        _                 -> return ()

portControlId :: JSString
portControlId = Config.prefix "focus-portcontrol"

inPortControl_ :: Ref App -> AnyPortRef -> Port -> ReactElementM ViewEventHandler ()
inPortControl_ ref portRef port = React.viewWithSKey inPortControl "inPortControl" (ref, portRef, port) mempty

inPortControl :: ReactView (Ref App, AnyPortRef, Port)
inPortControl = React.defineView "inPortControl" $ \(ref, portRef, port) ->
    div_
        [ "key"       $= jsShow (port ^. Port.portId)
        , "className" $= Style.prefixFromList [ "row", "row--arg" ]
        ] $ do
        div_
            [ "key"       $= "label"
            , "className" $= Style.prefix "label"
            ] $ elemString $ port ^. Port.name
        div_
            [ "key"       $= "value"
            , "className" $= Style.prefix "value"
            ] $
            case port ^. Port.state of
            PortAPI.NotConnected ->
                case port ^. Port.valueType . ValueType.toEnum of
                    ValueType.Other -> elemString "(other)"
                    _               -> do
                        let zeroValue    = case port ^. Port.valueType . ValueType.toEnum of
                                ValueType.DiscreteNumber   -> PortDefault.IntValue    def
                                ValueType.ContinuousNumber -> PortDefault.DoubleValue def
                                ValueType.String           -> PortDefault.StringValue def
                                ValueType.Bool             -> PortDefault.BoolValue   False
                                _                          -> undefined
                            defaultValue = PortDefault.Constant zeroValue
                        button_
                            [ onClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.PortSetPortDefault portRef defaultValue
                            ] $ elemString "not set"
            PortAPI.Connected -> elemString "(connected)"
            PortAPI.WithDefault defVal -> void $ case port ^. Port.valueType . ValueType.toEnum of
                ValueType.DiscreteNumber -> do
                    let value = fromMaybe 0 $ defVal ^? PortDefault._Constant . PortDefault._IntValue
                    div_
                        [ "className" $= Style.prefix "horizontal-slider"
                        --TODO[react]: +1 with Q and up key, -1 with W and down key, edit on double click
                        , onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.PortInitSlider m portRef $ Discrete value)
                        ] $ elemString $ show value
                ValueType.ContinuousNumber -> do
                    let value = fromMaybe 0.0 $ defVal ^? PortDefault._Constant . PortDefault._DoubleValue
                    div_
                        [ "className" $= Style.prefix "horizontal-slider"
                        --TODO[react]: +1 with Q and up key, -1 with W and down key, edit on double click
                        , onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.PortInitSlider m portRef $ Continous value)
                        ] $ elemString $ show value
                ValueType.String -> do
                    let value = fromMaybe "" $ defVal ^? PortDefault._Constant . PortDefault._StringValue
                        defaultValue val = PortDefault.Constant $ PortDefault.StringValue val
                    input_
                        [ "id" $= portControlId
                        , "value" $= convert value
                        , onMouseDown $ \e _ -> [stopPropagation e]
                        , onKeyDown   $ \e k -> let val = target e "value" in stopPropagation e : dispatch ref (UI.NodeEvent $ Node.PortApplyString k portRef $ defaultValue val)
                        , onChange    $ \e   -> let val = target e "value" in dispatch ref $ UI.NodeEvent $ Node.PortEditString portRef $ defaultValue val
                        ]
                ValueType.Bool -> do
                    let value = fromMaybe True $ defVal ^? PortDefault._Constant . PortDefault._BoolValue
                        defaultValue = PortDefault.Constant $ PortDefault.BoolValue $ not value
                    button_
                        [ onClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.PortSetPortDefault portRef defaultValue
                        ] $ elemString $ show value
                ValueType.Other ->
                    elemString "(other)"
