{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.PortControl
    ( portLabel_
    , portControl_
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

roundTo :: Int -> Double -> Double
roundTo limit a =  (round' $ f * a) / f
  where
    round' = fromIntegral . round
    f      = fromIntegral 10^limit

integer :: String -> String
integer a =
    if a==[] then "" else
        case last a of
            '.' -> init a
            _   -> integer $ init a

fractional :: String -> String
fractional a =
    case a of
        []     -> ""
        '.':xs -> (take 3 xs)
        _:xs   -> fractional xs

fractionalExp :: String -> String
fractionalExp a =
    case a of
        []     -> ""
        '.':xs -> (take 3 xs) ++ "…" ++ ePart xs
        _:xs   -> fractionalExp xs
    where
        ePart b =
            case b of
                []     -> ""
                'e':bs -> 'e':bs
                _:bs   -> ePart bs

isExpNotation :: String -> Bool
isExpNotation a =
    case a of
        []     -> False
        'e':xs -> True
        _:xs   -> isExpNotation xs

portControlId, labelPrefix, controlPrefix :: JSString
portControlId = Config.prefix "focus-portcontrol"
labelPrefix   = Config.prefix "label-"
controlPrefix = Config.prefix "control-"

portLabel_ :: InPort -> ReactElementM ViewEventHandler ()
portLabel_ port = case port ^. Port.portId of
    [Arg _] -> div_
                   [ "key"       $= (labelPrefix <> jsShow (port ^. Port.portId))
                   , "className" $= Style.prefix "node__label"
                   ] $ elemString . convert $ port ^. Port.name
    _       -> return ()

portControl_ :: Ref App -> NodeLoc -> InPort -> ReactElementM ViewEventHandler ()
portControl_ ref' nl' port' = React.viewWithSKey portControl (jsShow $ port' ^. Port.portId) (ref', nl', port') mempty
    where
        portControl = React.defineView "portControl" $ \(ref, nl, port) ->
            case port ^. Port.portId of
                [Arg _] -> do
                    let valueClass  = case port ^. Port.state of
                            PortAPI.NotConnected  -> "node__ctrl--not-connected"
                            PortAPI.Connected     -> "node__ctrl--connected"
                            PortAPI.WithDefault _ -> "node__ctrl--with-default"
                        portRef     = InPortRef nl $ port ^. Port.portId
                    div_
                        [ "key"       $= (controlPrefix <> jsShow (port ^. Port.portId))
                        , "className" $= Style.prefixFromList [ "node__ctrl", valueClass ]
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
                                        [ "className" $= Style.prefix "ctrl--set"
                                        , onClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.PortSetPortDefault portRef defaultValue
                                        ] $ elemString "set"
                        PortAPI.Connected -> elemString "" -- TODO get current value from the connected port
                        PortAPI.WithDefault defVal -> void $ case port ^. Port.valueType . ValueType.toEnum of
                            ValueType.DiscreteNumber -> do
                                let value       = fromMaybe 0 $ defVal ^? PortDefault._Constant . PortDefault._IntValue
                                div_
                                    [ "className" $= Style.prefix "ctrl--slider"
                                    --TODO[react]: +1 with Q and up key, -1 with W and down key, edit on double click
                                    , onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.PortInitSlider m portRef $ Discrete value)
                                    ] $ elemString $ show value
                            ValueType.ContinuousNumber -> do
                                let value = fromMaybe 0.0 $ defVal ^? PortDefault._Constant . PortDefault._DoubleValue
                                div_
                                    [ "className" $= Style.prefixFromList [ "ctrl--slider", "ctrl--slider--double" ]
                                    --TODO[react]: +1 with Q and up key, -1 with W and down key, edit on double click
                                    , onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.PortInitSlider m portRef $ Continous value)
                                    ] $ do
                                        let val = show $ roundTo 3 value
                                        div_ [ "className" $= Style.prefix "ctrl--slider__left"  ] $ elemString $ integer val
                                        div_ [ "className" $= Style.prefix "ctrl--slider__right" ] $ elemString $ (if isExpNotation val then fractionalExp else fractional) $ val
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

                _ -> div_ [ "key"       $= "self"
                          , "className" $= Style.prefixFromList ["node__label", "node__label--self"]
                          ] $ elemString "self"
