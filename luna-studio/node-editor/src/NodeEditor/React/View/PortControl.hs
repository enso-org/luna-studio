{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.PortControl
    ( portControl_
    ) where

import           Common.Prelude              hiding (group)
import           LunaStudio.Data.Port        (InPortIndex (Arg))
import qualified LunaStudio.Data.Port        as PortAPI
import qualified LunaStudio.Data.PortDefault as PortDefault
import           LunaStudio.Data.PortRef     (InPortRef (InPortRef))
import qualified LunaStudio.Data.ValueType   as ValueType
import qualified JS.Config                   as Config
import qualified NodeEditor.Event.UI         as UI
import qualified NodeEditor.React.Event.Node as Node
import           NodeEditor.React.Model.App  (App)
import           NodeEditor.React.Model.Node (NodeLoc)
import           NodeEditor.React.Model.Port (InPort)
import qualified NodeEditor.React.Model.Port as Port
import           NodeEditor.React.Store      (Ref, dispatch)
import qualified NodeEditor.React.View.Style as Style
import           NodeEditor.State.Action     (InitValue (Continous, Discrete))
import           React.Flux                  as React




portControl_ :: Ref App -> NodeLoc -> InPort -> ReactElementM ViewEventHandler ()
portControl_ ref nl port = React.viewWithSKey portControl (jsShow $ port ^. Port.portId) (ref, nl, port) mempty

portControl :: ReactView (Ref App, NodeLoc, InPort)
portControl = React.defineView "portControl" $ \(ref, nl, port) ->
    case port ^. Port.portId of
        [Arg _] -> inPortControl_ ref (InPortRef nl $ port ^. Port.portId) port
        _       -> return ()

portControlId :: JSString
portControlId = Config.prefix "focus-portcontrol"

inPortControl_ :: Ref App -> InPortRef -> InPort -> ReactElementM ViewEventHandler ()
inPortControl_ ref portRef port = React.viewWithSKey inPortControl "inPortControl" (ref, portRef, port) mempty

inPortControl :: ReactView (Ref App, InPortRef, InPort)
inPortControl = React.defineView "inPortControl" $ \(ref, portRef, port) -> do
    let valueClass = case port ^. Port.state of
                          PortAPI.NotConnected  -> "value--not-connected"
                          PortAPI.Connected     -> "value--connected"
                          PortAPI.WithDefault _ -> "value--with-default"
    div_
        [ "key"       $= jsShow (port ^. Port.portId)
        , "className" $= Style.prefixFromList [ "row", "row--arg" ]
        ] $ do
        div_
            [ "key"       $= "label"
            , "className" $= Style.prefix "label"
            ] $ elemString . convert $ port ^. Port.name
        div_
            [ "key"       $= "value"
            , "className" $= Style.prefixFromList ["value", valueClass]
            ] $
            case port ^. Port.state of
            PortAPI.NotConnected ->
                case port ^. Port.valueType . ValueType.toEnum of
                    ValueType.Other -> elemString ""
                    _               -> do
                        let zeroValue    = case port ^. Port.valueType . ValueType.toEnum of
                                ValueType.DiscreteNumber   -> PortDefault.IntValue    def
                                ValueType.ContinuousNumber -> PortDefault.DoubleValue def
                                ValueType.String           -> PortDefault.StringValue def
                                ValueType.Bool             -> PortDefault.BoolValue   False
                                _                          -> undefined
                            defaultValue = PortDefault.Constant zeroValue
                        button_
                            [ "className" $= Style.prefix "ctrl-set"
                            , onClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.PortSetPortDefault portRef defaultValue
                            ] $ elemString "set"
            PortAPI.Connected -> elemString "(connected)"
            PortAPI.WithDefault defVal -> void $ case port ^. Port.valueType . ValueType.toEnum of
                ValueType.DiscreteNumber -> do
                    let value       = fromMaybe 0 $ defVal ^? PortDefault._Constant . PortDefault._IntValue
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
                ValueType.Other -> elemString ""
