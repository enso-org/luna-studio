{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.PortControl
    ( portControl_
    ) where

import           Common.Prelude              hiding (group)
import           JS.Key                      (toKey)
import qualified JS.Mount                    as Mount
import           LunaStudio.Data.Port        (InPortIndex (Arg))
import qualified LunaStudio.Data.Port        as PortAPI
import qualified LunaStudio.Data.PortDefault as PortDefault
import           LunaStudio.Data.PortRef     (InPortRef (InPortRef))
import           LunaStudio.Data.TypeRep     (TypeRep (TCons))
import           NodeEditor.Data.Slider      (InitValue (Continous, Discrete))
import qualified NodeEditor.Event.UI         as UI
import qualified NodeEditor.React.Event.Port as Port
import           NodeEditor.React.IsRef      (IsRef, dispatch)
import           NodeEditor.React.Model.Node (NodeLoc)
import           NodeEditor.React.Model.Port (InPort)
import qualified NodeEditor.React.Model.Port as Port
import qualified NodeEditor.React.View.Style as Style
import           React.Flux                  as React


roundTo :: Int -> Double -> Double
roundTo limit a =  round' (f * a) / f
  where
    round' v = fromIntegral (round v :: Integer)
    f        = fromIntegral (10^limit :: Integer)

integer :: String -> String
integer a = if null a then "" else case last a of
    '.' -> init a
    _   -> integer $ init a

fractional :: String -> String
fractional a = case a of
    []     -> ""
    '.':xs -> take 3 xs
    _:xs   -> fractional xs

fractionalExp :: String -> String
fractionalExp a = case a of
        []     -> ""
        '.':xs -> take 3 xs <> "…" <> ePart xs
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
        []    -> False
        'e':_ -> True
        _:xs  -> isExpNotation xs

labelPrefix, controlPrefix :: JSString
labelPrefix   = Mount.prefix "label-"
controlPrefix = Mount.prefix "control-"

portControlId :: InPortRef -> JSString
portControlId portRef = Mount.prefix "focus-portcontrol" <> toKey portRef

portControl_ :: IsRef r => r -> NodeLoc -> InPort -> Bool -> ReactElementM ViewEventHandler ()
portControl_ ref' nl' port' isTextEdited = React.viewWithSKey portControl (jsShow $ port' ^. Port.portId) (ref', nl', port', isTextEdited) mempty

portControl :: IsRef r => ReactView (r, NodeLoc, InPort, Bool)
portControl = React.defineView "portControl" $ \(ref, nl, port, isTextEdited) ->
    case port ^. Port.portId of
        [Arg _] -> row ["node__control"] $ do
                        div_
                           [ "key"       $= (labelPrefix <> jsShow (port ^. Port.portId))
                           , "className" $= Style.prefix "node__label"
                           ] $ elemString . convert $ port ^. Port.name
                        portCtrl ref nl port isTextEdited False

        []      -> row ["node__control", "node__control--alias"] $ do
                        div_
                           [ "key"       $= "alias-label"
                           , "className" $= Style.prefix "node__label"
                           ] $ elemString "alias"
                        portCtrl ref nl port isTextEdited True
        _       -> row ["node__control", "node__control--self"] $ do
                        div_
                           [ "key"       $= "self-label"
                           , "className" $= Style.prefix "node__label"
                           ] $ elemString "self"
                        return ()

    where
        row classList = div_ [ "className" $= Style.prefixFromList classList ]

        portCtrl ref nl port isTextEdited isAlias = do
            let valueClass  = case port ^. Port.state of
                    PortAPI.NotConnected  -> "node__ctrl--not-connected"
                    PortAPI.Connected     -> "node__ctrl--connected"
                    PortAPI.WithDefault _ -> "node__ctrl--with-default"
                portRef = InPortRef nl $ port ^. Port.portId
            div_
                [ "key"       $= (controlPrefix <> jsShow (port ^. Port.portId))
                , "className" $= ("native-key-bindings " <> Style.prefixFromList [ "node__ctrl", valueClass, if isAlias then "node__ctrl--alias" else "" ])
                ] $ case port ^. Port.state of
                    PortAPI.NotConnected -> do
                        let tConsElem :: PortDefault.PortValue -> ReactElementM ViewEventHandler ()
                            tConsElem zeroValue = button_
                                [ "className" $= Style.prefix "ctrl--set"
                                , onClick $ \_ _ -> dispatch ref $ UI.PortEvent $ Port.PortSetPortDefault portRef $ PortDefault.Constant zeroValue
                                ] $ elemString "set"
                        case port ^. Port.valueType of
                            TCons "Int"  _ -> tConsElem $ PortDefault.IntValue  def
                            TCons "Real" _ -> tConsElem $ PortDefault.RealValue def
                            TCons "Text" _ -> tConsElem $ PortDefault.TextValue def
                            TCons "Bool" _ -> tConsElem $ PortDefault.BoolValue False
                            _              -> elemString ""
                    PortAPI.Connected -> elemString "" -- TODO get current value from the connected port
                    PortAPI.WithDefault defVal -> void $ case port ^. Port.valueType of
                        TCons "Int" _ -> do
                            let value = fromMaybe 0 $ defVal ^? PortDefault._Constant . PortDefault._IntValue
                            div_
                                [ "className" $= Style.prefixFromList [ "ctrl--slider", "ctrl--slider--int" ]
                                --TODO[react]: +1 with Q and up key, -1 with W and down key, edit on double click
                                , onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.PortEvent $ Port.PortInitSlider m portRef $ Discrete value)
                                ] $ elemString $ show value
                        TCons "Real" _ -> do
                            let value = fromMaybe 0.0 $ defVal ^? PortDefault._Constant . PortDefault._RealValue
                            div_
                                [ "className" $= Style.prefixFromList [ "ctrl--slider", "ctrl--slider--real" ]
                                --TODO[react]: +1 with Q and up key, -1 with W and down key, edit on double click
                                , onMouseDown $ \e m -> stopPropagation e : dispatch ref (UI.PortEvent $ Port.PortInitSlider m portRef $ Continous value)
                                ] $ do
                                    let val = show $ roundTo 3 value
                                    div_ [ "className" $= Style.prefix "ctrl--slider__left"  ] $ elemString $ integer val
                                    div_ [ "className" $= Style.prefix "ctrl--slider__right" ] $ elemString $ (if isExpNotation val then fractionalExp else fractional) val
                        TCons "Text" _ -> do
                            let value            = fromMaybe "" $ defVal ^? PortDefault._Constant . PortDefault._TextValue
                                defaultValue val = PortDefault.Constant $ PortDefault.TextValue val
                                className        = Style.prefixFromList ["ctrl--text", "input"]
                                valProp          = if isTextEdited then "defaultValue" else "value"
                            input_
                                [ "id"        $= portControlId portRef
                                , "className" $= className
                                , valProp     $= convert value
                                , onMouseDown $ \e _ -> [stopPropagation e]
                                , onFocus     $ \e _ -> dispatch ref (UI.PortEvent $ Port.EditTextPortControl portRef (convert value))
                                , onChange    $ \e   -> let val = target e "value" in
                                                     dispatch ref (UI.PortEvent $ Port.EditTextPortControl portRef val)
                                , onBlur      $ \e _ -> dispatch ref (UI.PortEvent $ Port.EditTextPortControlBlur portRef)
                                ]
                        TCons "Bool" _ -> do
                            let value = fromMaybe True $ defVal ^? PortDefault._Constant . PortDefault._BoolValue
                                defaultValue = PortDefault.Constant $ PortDefault.BoolValue $ not value
                            div_
                                [ onClick $ \_ _ -> dispatch ref $ UI.PortEvent $ Port.PortSetPortDefault portRef defaultValue
                                , "className" $= Style.prefix ("ctrl--bool--" <> jsShow value)
                                ] mempty
                        _ -> elemString ""


