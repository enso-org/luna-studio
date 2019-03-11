module NodeEditor.React.View.Port where

import Common.Prelude

import qualified Data.Aeson                  as Aeson
import qualified NodeEditor.Event.Mouse      as Mouse
import qualified NodeEditor.Event.UI         as UI
import qualified NodeEditor.React.Event.Port as Port
import qualified NodeEditor.React.Model.Port as Port
import qualified NodeEditor.React.View.Style as Style
import qualified React.Flux                  as React

import LunaStudio.Data.Angle            (Angle)
import LunaStudio.Data.Constants        (nodePropertiesWidth)
import LunaStudio.Data.PortRef          (AnyPortRef (InPortRef'), InPortRef,
                                         toAnyPortRef)
import NodeEditor.React.IsRef           (IsRef, dispatch)
import NodeEditor.React.Model.Constants (connectionWidth, lineHeight,
                                         nodeRadius, nodeRadius',
                                         portAliasRadius, portRadius)
import NodeEditor.React.Model.Node      (NodeLoc)
import NodeEditor.React.Model.Port      (AnyPort,
                                         AnyPortId (InPortId', OutPortId'),
                                         InPortIndex (Self), IsOnly, Mode (..),
                                         argumentConstructorOffsetY, isInPort,
                                         isInvisible, portAngleStart,
                                         portAngleStop)
import Numeric                          (showFFloat)
import React.Flux                       hiding (view)


name :: JSString
name = "port"

typeOffsetX, typeOffsetY :: Double
typeOffsetX = 36
typeOffsetY = (-3.5)

selectAreaWidth :: Double
selectAreaWidth = 8

modeClass :: Mode -> [String]
modeClass Invisible      = ["port--invisible"]
modeClass Inactive       = ["port--inactive"]
modeClass TypeNotMatched = ["port--type-not-matched"]
modeClass Highlighted    = ["port--highlighted"]
modeClass _              = []

jsShow2 :: Double -> JSString
jsShow2 a = convert $ showFFloat (Just 2) a "" -- limits Double to two decimal numbers

handleMouseDown :: IsRef r => r -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseDown ref portRef e m =
    if Mouse.withoutMods m Mouse.leftButton then
        stopPropagation e : dispatch ref (UI.PortEvent $ Port.MouseDown m portRef)
    else []

handleClick :: IsRef r => r -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleClick ref portRef e m =
    if Mouse.withoutMods m Mouse.leftButton then
        stopPropagation e : dispatch ref (UI.PortEvent $ Port.Click m portRef)
    else []

handleMouseUp :: IsRef r => r -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseUp ref portRef _ _ = dispatch ref (UI.PortEvent $ Port.MouseUp portRef)

handleMouseEnter :: IsRef r => r -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseEnter ref portRef _ _ = dispatch ref (UI.PortEvent $ Port.MouseEnter portRef)

handleMouseLeave :: IsRef r => r -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseLeave ref portRef _ _ = dispatch ref (UI.PortEvent $ Port.MouseLeave portRef)

port :: IsRef r => ReactView (r, NodeLoc, Int, Int, IsOnly, AnyPort, Bool)
port = React.defineView name $ \(ref, nl, num, numOfArgs, isOnly, p, isTopLevel) ->
    case p ^. Port.portId of
        InPortId' []       -> portAlias_ ref nl p isTopLevel
        InPortId' (Self:_) -> portSelf_  ref nl p isTopLevel
        OutPortId' []      -> if isOnly then portSingle_ ref nl p isTopLevel
                                        else portIO_ ref nl p num numOfArgs isTopLevel
        _                  ->                portIO_ ref nl p num numOfArgs isTopLevel

portExpanded :: IsRef r => ReactView (r, NodeLoc, AnyPort, Int)
portExpanded = React.defineView name $ \(ref, nl, p, num) -> portIOExpanded_ ref nl p num

port_ :: IsRef r => r -> NodeLoc -> AnyPort -> Int -> Int -> IsOnly -> Bool -> ReactElementM ViewEventHandler ()
port_ ref nl p num numOfArgs isOnly isTopLevel =
    React.viewWithSKey port (jsShow $ p ^. Port.portId) (ref, nl, num, numOfArgs, isOnly, p, isTopLevel) mempty

portExpanded_ :: IsRef r => r -> NodeLoc -> AnyPort -> Int -> ReactElementM ViewEventHandler ()
portExpanded_ ref nl p num =
    React.viewWithSKey portExpanded (jsShow $ p ^. Port.portId) (ref, nl, p, num) mempty

handlers :: IsRef r => r -> AnyPortRef -> Bool -> [PropertyOrHandler [SomeStoreAction]]
handlers ref portRef isTopLevel = [ onMouseEnter $ handleMouseEnter ref portRef
                                  , onMouseLeave $ handleMouseLeave ref portRef] <> handlers' where
    handlers' = if isTopLevel then [] else  [ onMouseDown  $ handleMouseDown  ref portRef
                                            , onMouseUp    $ handleMouseUp    ref portRef
                                            , onClick      $ handleClick      ref portRef ]

portAlias_ :: IsRef r => r -> NodeLoc -> AnyPort -> Bool -> ReactElementM ViewEventHandler ()
portAlias_ ref nl p isTopLevel = React.viewWithSKey portAlias "port-alias" (ref, nl, p, isTopLevel) mempty

portSelf_ :: IsRef r => r -> NodeLoc -> AnyPort -> Bool -> ReactElementM ViewEventHandler ()
portSelf_ ref nl p isTopLevel = React.viewWithSKey portSelf "port-self" (ref, nl, p, isTopLevel) mempty

portSingle_ :: IsRef r => r -> NodeLoc -> AnyPort -> Bool -> ReactElementM ViewEventHandler ()
portSingle_ ref nl p isTopLevel = React.viewWithSKey portSingle "port-single" (ref, nl, p, isTopLevel) mempty

portIO_ :: IsRef r => r -> NodeLoc -> AnyPort -> Int -> Int -> Bool -> ReactElementM ViewEventHandler ()
portIO_ ref nl p num numOfArgs isTopLevel = React.viewWithSKey portIO "port-io" (ref, nl, p, num, numOfArgs, isTopLevel) mempty

portAlias :: IsRef r => ReactView (r, NodeLoc, AnyPort, Bool)
portAlias = React.defineView "port-alias" $ \(ref, nl, p, isTopLevel) -> do
    let portId       = p ^. Port.portId
        portRef      = toAnyPortRef nl portId
        color        = convert $ p ^. Port.color
        className    = Style.prefixFromList $ ["port", "port--alias"] <> modeClass (p ^. Port.mode)
        portHandlers = if isInvisible p then [] else handlers ref portRef isTopLevel
    g_
        [ "className" $= className ] $ do
        circle_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> "a")
            , "r"         $= jsShow2 portAliasRadius
            , "stroke"    $= color
            ] mempty
        circle_
            ( portHandlers <>
            [ "className"      $= Style.prefix "port__select"
            , "key"            $= (jsShow portId <> "b")
            , "r"              $= jsShow2 portAliasRadius
            , "strokeWidth"    $= jsShow2 (portRadius - portAliasRadius - connectionWidth)
            , "strokeLocation" $= "outside"
            ]) mempty

portSelf :: IsRef r => ReactView (r, NodeLoc, AnyPort, Bool)
portSelf = React.defineView "port-self" $ \(ref, nl, p, isTopLevel) -> do
    let portId       = p ^. Port.portId
        portRef      = toAnyPortRef nl portId
        color        = convert $ p ^. Port.color
        className    = Style.prefixFromList $ ["port", "port--self"] <> modeClass (p ^. Port.mode)
        portHandlers = if isInvisible p then [] else handlers ref portRef isTopLevel
    g_
        [ "className" $= className ] $ do
        circle_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> "a")
            , "fill"      $= color
            ] mempty
        circle_
            ( portHandlers <>
            [ "className" $= Style.prefix "port__select"
            , "key"       $= (jsShow portId <> "b")
            , "r"         $= jsShow2 (portAliasRadius - connectionWidth/2)
            ]) mempty

portSingle :: IsRef r => ReactView (r, NodeLoc, AnyPort, Bool)
portSingle = React.defineView "port-single" $ \(ref, nl, p, isTopLevel) -> do
    let portId   = p ^. Port.portId
        portRef  = toAnyPortRef nl portId
        portType = toString $ p ^. Port.valueType
        isInput  = isInPort portId
        color    = convert $ p ^. Port.color
        classes  = Style.prefixFromList $ [ "port", "port--o", "port--o--single" ] <> modeClass (p ^. Port.mode)
        portTypeClass = if isTopLevel then "hide" else "port__type"
        r1 :: Double -> JSString
        r1 = jsShow2 . (+) nodeRadius
        r2 = jsShow2 nodeRadius'
        svgPath :: Double -> Integer -> Integer -> JSString
        svgPath a b c = "M0 -" <> r1 a <> " A " <> r1 a <> " " <> r1 a <> " 0 0 " <> jsShow b <> " 0 "  <> r1 a <>
                       " L0 "  <> r2   <> " A " <> r2   <> " " <> r2   <> " 1 0 " <> jsShow c <> " 0 -" <> r2   <> " Z "
    g_ [ "className" $= classes ] $ do
        text_
            [ "className" $= Style.prefixFromList ([ portTypeClass, "noselect" ] <> modeClass (p ^. Port.mode))
            , "key"       $= (jsShow portId <> "-type")
            , "y"         $= jsShow2 (-typeOffsetY)
            , "x"         $= jsShow2 (if isInput then (-typeOffsetX) else typeOffsetX)
            ] $ elemString portType
        path_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> "a" )
            , "fill"      $= color
            , "d"         $= (svgPath 20 0 1 <> svgPath 20 1 0)
            ] mempty
        path_
            ( handlers ref portRef isTopLevel <>
            [ "className" $= Style.prefix "port__select"
            , "key"       $= (jsShow portId <> "b")
            , "d"         $= (svgPath 20 0 1 <> svgPath 20 1 0)
            ]) mempty

portIO :: IsRef r => ReactView (r, NodeLoc, AnyPort, Int, Int, Bool)
portIO = React.defineView "port-io" $ \(ref, nl, p, num, numOfArgs, isTopLevel) -> do
    let portId   = p ^. Port.portId
        portRef  = toAnyPortRef nl portId
        portType = toString $ p ^. Port.valueType
        isInput  = isInPort portId
        color    = convert $ p ^. Port.color
        classes  = if isInput then [ "port", "port--i", "port--i--" <> show (num + 1) ] <> modeClass (p ^. Port.mode)
                              else [ "port", "port--o", "port--o--" <> show (num + 1) ] <> modeClass (p ^. Port.mode)
        portTypeClass = if isTopLevel then "hide" else "port__type"
        svgFlag1 = if isInput then "0"  else "1"
        svgFlag2 = if isInput then "1"  else "0"
        mode     = if isInput then -1.0 else 1.0
        adjust = typeOffsetY + fromIntegral ((min 3 (numOfArgs - 1)) * 8)
        startPortArcX addGaps r = r * sin(toSvgAngle $ portAngleStart addGaps num numOfArgs r * mode)
        startPortArcY addGaps r = r * cos(toSvgAngle $ portAngleStart addGaps num numOfArgs r * mode)
        stopPortArcX  addGaps r = r * sin(toSvgAngle $ portAngleStop  addGaps num numOfArgs r * mode)
        stopPortArcY  addGaps r = r * cos(toSvgAngle $ portAngleStop  addGaps num numOfArgs r * mode)
        ax addGaps = jsShow2 . startPortArcX addGaps . (+) nodeRadius
        ay addGaps = jsShow2 . startPortArcY addGaps . (+) nodeRadius
        bx addGaps = jsShow2 . stopPortArcX  addGaps . (+) nodeRadius
        bY addGaps = jsShow2 . stopPortArcY  addGaps . (+) nodeRadius
        cx addGaps = jsShow2 $ stopPortArcX  addGaps nodeRadius'
        cy addGaps = jsShow2 $ stopPortArcY  addGaps nodeRadius'
        dx addGaps = jsShow2 $ startPortArcX addGaps nodeRadius'
        dy addGaps = jsShow2 $ startPortArcY addGaps nodeRadius'
        r1 = jsShow2 . (+) nodeRadius
        r2 = jsShow2 nodeRadius'
        svgPath a b = "M"  <> ax a b <> " " <> ay a b <>
                     " A " <> r1 b   <> " " <> r1 b   <> " 0 0 " <> svgFlag1 <> " " <> bx a b <> " " <> bY a b <>
                     " L " <> cx a   <> " " <> cy a   <>
                     " A " <> r2     <> " " <> r2     <> " 0 0 " <> svgFlag2 <> " " <> dx a   <> " " <> dy a   <>
                     " Z"
    g_
        [ "className" $= Style.prefixFromList classes
        ] $ do
        text_
            [ "className" $= Style.prefixFromList ([ portTypeClass, "noselect" ] <> modeClass (p ^. Port.mode))
            , "key"       $= (jsShow portId <> "-type")
            , "y"         $= jsShow2 ((lineHeight * fromIntegral num) - adjust)
            , "x"         $= jsShow2 (if isInput then (-typeOffsetX) else typeOffsetX)
            ] $ elemString portType
        path_
            [ "className"   $= Style.prefix "port__shape"
            , "key"         $= (jsShow portId <> "-shape")
            , "fill"        $= color
            , "d"           $= svgPath True 20
            ] mempty
        path_
            ( handlers ref portRef isTopLevel <>
              [ "className" $= Style.prefix "port__select"
              , "key"       $= (jsShow portId <> "-select")
              , "d"         $= svgPath False lineHeight
              ]
            ) mempty

portIOExpanded_ :: IsRef r => r -> NodeLoc -> AnyPort -> Int -> ReactElementM ViewEventHandler ()
portIOExpanded_ ref nl p num = do
    let portId   = p ^. Port.portId
        portRef  = toAnyPortRef nl portId
        portType = toString $ p ^. Port.valueType
        isInput  = isInPort portId
        color    = convert $ p ^. Port.color
        px       = jsShow2 (if isInput then (-nodePropertiesWidth/2) else nodePropertiesWidth/2)
        py       = jsShow2 (lineHeight * fromIntegral num)
        classes  = if isInput then [ "port", "port--i", "port--i--" <> show (num + 1)] <> modeClass (p ^. Port.mode)
                              else [ "port", "port--o", "port--o--" <> show (num + 1)] <> modeClass (p ^. Port.mode)
    g_
        [ "className" $= Style.prefixFromList classes
        ] $ do
        text_
            [ "className" $= Style.prefixFromList ([ "port__type", "noselect" ] <> modeClass (p ^. Port.mode))
            , "key"       $= (jsShow portId <> "-type")
            , "y"         $= py
            , "dy"        $= "4px"
            , "dx"        $= jsShow2 (if isInput then (-(16 + nodePropertiesWidth/2)) else 16 + nodePropertiesWidth/2 )
            ] $ elemString portType
        circle_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> jsShow num <> "-shape")
            , "fill"      $= color
            , "r"         $= jsShow2 3
            , "cx"        $= (px <> "px")
            , "cy"        $= (py <> "px")
            ] mempty
        circle_
            ( handlers ref portRef False <>
              [ "className" $= Style.prefix "port__select"
              , "key"       $= (jsShow portId <> jsShow num <> "-select")
              , "r"         $= jsShow2 (lineHeight/1.5)
              , "cx"        $= (px <> "px")
              , "cy"        $= (py <> "px")
              ]
            ) mempty

argumentConstructor_ :: IsRef r => r -> InPortRef -> Int -> Bool -> Bool -> Bool -> ReactElementM ViewEventHandler ()
argumentConstructor_ ref portRef numOfPorts isConnectionSource hasAlias hasSelf = do
    let offsetY   = argumentConstructorOffsetY numOfPorts
        highlight = if isConnectionSource then ["port--highlighted"] else []
    g_
        [ "key"       $= "argument-constructor"
        , "className" $= Style.prefixFromList (["port", "port--i", "port--i--constructor"] <> highlight)
        ] $ do
        circle_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= "shape"
            , "style"     @= Aeson.object [ "cy" Aeson..= (show offsetY <> "px") ]
            , "r"         $= jsShow2 3
            ] mempty
        circle_
            ( handlers ref (InPortRef' portRef) False <>
                [ "className" $= Style.prefix "port__select"
                , "key"       $= "select"
                , "r"         $= jsShow2 (lineHeight/1.5)
                , "style"     @= Aeson.object [ "cy" Aeson..= (show offsetY <> "px") ]
                ]
            ) mempty

toSvgAngle :: Angle -> Angle
toSvgAngle = (-) pi
