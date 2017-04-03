{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Port where

import           Luna.Studio.Action.State.Model    (portAngleStart, portAngleStop)
import           Luna.Studio.Data.PortRef          (AnyPortRef, toAnyPortRef)
import qualified Luna.Studio.Event.Mouse           as Mouse
import qualified Luna.Studio.Event.UI              as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Port      as Port
import           Luna.Studio.React.Model.App       (App)
import           Luna.Studio.React.Model.Constants (lineHeight, nodeRadius, nodeRadius')
import           Luna.Studio.React.Model.Node      (NodeLoc)
--import           Luna.Studio.React.Model.Port (Port (..))
import           Luna.Studio.React.Model.Port      (InPort (Self), Mode (Highlighted, Invisible), OutPort (All), Port,
                                                    PortId (InPortId, OutPortId), getPortNumber, isHighlighted, isInPort)
import qualified Luna.Studio.React.Model.Port      as Port
import           Luna.Studio.React.Store           (Ref, dispatch)
import qualified Luna.Studio.React.View.Style      as Style
import           Numeric                           (showFFloat)
import           React.Flux                        hiding (view)
import qualified React.Flux                        as React

name :: JSString
name = "port"

typeOffsetX :: Double
typeOffsetX = 40

typeOffsetY1,typeOffsetY2,typeOffsetY3,typeOffsetY :: Double
typeOffsetY1 = (-3.5)
typeOffsetY2 = 4.5
typeOffsetY3 = 12.5
typeOffsetY  = 20.5

selectAreaWidth :: Double
selectAreaWidth = 8

jsShow2 :: Double -> JSString
jsShow2 a = convert $ showFFloat (Just 2) a "" -- limit Double to two decimal numbers

handleMouseDown :: Ref App -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseDown ref portRef e m =
    if Mouse.withoutMods m Mouse.leftButton then
        stopPropagation e : dispatch ref (UI.PortEvent $ Port.MouseDown m portRef)
    else []

handleClick :: Ref App -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleClick ref portRef e m =
    if Mouse.withoutMods m Mouse.leftButton then
        stopPropagation e : dispatch ref (UI.PortEvent $ Port.Click m portRef)
    else []

handleMouseUp :: Ref App -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseUp ref portRef _ _ = dispatch ref (UI.PortEvent $ Port.MouseUp portRef)

handleMouseEnter :: Ref App -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseEnter ref portRef _ _ = dispatch ref (UI.PortEvent $ Port.MouseEnter portRef)

handleMouseLeave :: Ref App -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseLeave ref portRef _ _ = dispatch ref (UI.PortEvent $ Port.MouseLeave portRef)


port :: ReactView (Ref App, NodeLoc, Int, Bool, Port)
port = React.defineView name $ \(ref, nl, numOfPorts, isOnly, p) ->
    case p ^. Port.portId of
        InPortId   Self          ->                portSelf_   ref nl p
        OutPortId  All           -> if isOnly then portSingle_ ref nl p
                                    else           portIO_     ref nl p numOfPorts
        _                        ->                portIO_     ref nl p numOfPorts

portExpanded :: ReactView (Ref App, NodeLoc, Port)
portExpanded = React.defineView name $ \(ref, nl, p) ->
    case p ^. Port.portId of
        InPortId   Self          -> portSelf_       ref nl p
        _                        -> portIOExpanded_ ref nl p

port_ :: Ref App -> NodeLoc -> Port -> Int -> Bool -> ReactElementM ViewEventHandler ()
port_ ref nl p numOfPorts isOnly =
    React.viewWithSKey port (jsShow $ p ^. Port.portId) (ref, nl, numOfPorts, isOnly, p) mempty

portExpanded_ :: Ref App -> NodeLoc -> Port -> ReactElementM ViewEventHandler ()
portExpanded_ ref nl p =
    React.viewWithSKey portExpanded (jsShow $ p ^. Port.portId) (ref, nl, p) mempty

handlers :: Ref App -> AnyPortRef -> [PropertyOrHandler [SomeStoreAction]]
handlers ref portRef = [ onMouseDown  $ handleMouseDown  ref portRef
                       , onMouseUp    $ handleMouseUp    ref portRef
                       , onClick      $ handleClick      ref portRef
                       , onMouseEnter $ handleMouseEnter ref portRef
                       , onMouseLeave $ handleMouseLeave ref portRef
                       ]

portSelf_ :: Ref App -> NodeLoc -> Port -> ReactElementM ViewEventHandler ()
portSelf_ ref nl p = do
    let portId    = p ^. Port.portId
        portRef   = toAnyPortRef nl portId
        color     = convert $ p ^. Port.color
        modeClass = case p ^. Port.mode of
            Highlighted -> ["hover"]
            Invisible   -> ["port--invisible"]
            _           -> []
        className = Style.prefixFromList $ ["port", "port--self"] ++ modeClass
    g_
        [ "className" $= className ] $ do
        circle_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> "a")
            , "fill"      $= color
            ] mempty
        circle_
            ( handlers ref portRef ++
            [ "className" $= Style.prefix "port__select"
            , "key"       $= (jsShow portId <> "b")
            , "r"         $= jsShow2 (lineHeight/1.5)
            ]) mempty

portSingle_ :: Ref App -> NodeLoc -> Port -> ReactElementM ViewEventHandler ()
portSingle_ ref nl p = do
    let portId    = p ^. Port.portId
        portRef   = toAnyPortRef nl portId
        portType  = toString $ p ^. Port.valueType
        isInput   = isInPort portId
        color     = convert $ p ^. Port.color
        classes   = Style.prefixFromList $ [ "port", "port--o--single" ] ++ if isHighlighted p then ["hover"] else []
        r1 :: Double -> JSString
        r1 = jsShow2 . (+) nodeRadius
        r2 = jsShow2 nodeRadius'
        svgPath :: Double -> Integer -> Integer -> JSString
        svgPath a b c = "M0 -" <> r1 a <> " A " <> r1 a <> " " <> r1 a <> " 0 0 " <> jsShow b <> " 0 "  <> r1 a <>
                       " L0 "  <> r2   <> " A " <> r2   <> " " <> r2   <> " 1 0 " <> jsShow c <> " 0 -" <> r2   <> " Z "
    g_ [ "className" $= classes ] $ do
        text_
            [ "className" $= Style.prefixFromList [ "port__type", "noselect" ]
            , "key"       $= (jsShow portId <> "-type")
            , "y"         $= jsShow2 (-typeOffsetY1)
            , "x"         $= jsShow2 (if isInput then (-typeOffsetX) else typeOffsetX)
            ] $ elemString portType
        path_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> "a" )
            , "fill"      $= color
            , "d"         $= (svgPath 20 0 1 <> svgPath 20 1 0)
            ] mempty
        path_
            ( handlers ref portRef ++
              [ "className" $= Style.prefix "port__select"
              , "key"       $= (jsShow portId <> "b")
              , "d"         $= (svgPath 20 0 1 <> svgPath 20 1 0)
              ]
            ) mempty

portIO_ :: Ref App -> NodeLoc -> Port -> Int -> ReactElementM ViewEventHandler ()
portIO_ ref nl p numOfPorts = do
    let portId    = p ^. Port.portId
        portRef   = toAnyPortRef nl portId
        portType  = toString $ p ^. Port.valueType
        isInput   = isInPort portId
        num       = getPortNumber portId
        color     = convert $ p ^. Port.color
        highlight = if isHighlighted p then ["hover"] else []
        classes   = if isInput then [ "port", "port--i", "port--i--" <> show (num + 1) ] ++ highlight
                               else [ "port", "port--o", "port--o--" <> show (num + 1) ] ++ highlight
        svgFlag1  = if isInput then "1"  else "0"
        svgFlag2  = if isInput then "0"  else "1"
        mode      = if isInput then -1.0 else 1.0
--      n         = if isInput then 1 else 0
        adjust
            | numOfPorts == 1 = typeOffsetY1
            | numOfPorts == 2 = typeOffsetY2
            | numOfPorts == 3 = typeOffsetY3
            | otherwise       = typeOffsetY
        startPortArcX isShape r = r * sin(portAngleStart isShape num numOfPorts r * mode)
        startPortArcY isShape r = r * cos(portAngleStart isShape num numOfPorts r * mode)
        stopPortArcX  isShape r = r * sin(portAngleStop  isShape num numOfPorts r * mode)
        stopPortArcY  isShape r = r * cos(portAngleStop  isShape num numOfPorts r * mode)
        ax isShape = jsShow2 . startPortArcX isShape . (+) nodeRadius
        ay isShape = jsShow2 . startPortArcY isShape . (+) nodeRadius
        bx isShape = jsShow2 . stopPortArcX  isShape . (+) nodeRadius
        by isShape = jsShow2 . stopPortArcY  isShape . (+) nodeRadius
        cx isShape = jsShow2 $ stopPortArcX  isShape nodeRadius'
        cy isShape = jsShow2 $ stopPortArcY  isShape nodeRadius'
        dx isShape = jsShow2 $ startPortArcX isShape nodeRadius'
        dy isShape = jsShow2 $ startPortArcY isShape nodeRadius'
        r1 = jsShow2 . (+) nodeRadius
        r2 = jsShow2 nodeRadius'
        svgPath a b = "M"  <> ax a b <> " " <> ay a b <>
                     " A " <> r1 b <> " " <> r1 b <> " 0 0 " <> svgFlag1 <> " " <> bx a b <> " " <> by a b <>
                     " L " <> cx a <> " " <> cy a <>
                     " A " <> r2   <> " " <> r2   <> " 0 0 " <> svgFlag2 <> " " <> dx a   <> " " <> dy a   <>
                     " Z"
    g_
        [ "className" $= Style.prefixFromList classes
        ] $ do
        text_
            [ "className" $= Style.prefixFromList [ "port__type", "noselect" ]
            , "key"       $= (jsShow portId <> "-type")
            , "y"         $= jsShow2 ((lineHeight * fromIntegral num) - adjust)
            , "x"         $= jsShow2 (if isInput then (-typeOffsetX) else typeOffsetX)
            ] $ elemString portType
        path_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> "-shape")
            , "fill"      $= color
            , "d"         $= svgPath True 20
            ] mempty
        path_
            ( handlers ref portRef ++
              [ "className" $= Style.prefix "port__select"
              , "key"       $= (jsShow portId <> "-select")
              , "d"         $= svgPath False lineHeight
              ]
            ) mempty

portIOExpanded_ :: Ref App -> NodeLoc -> Port -> ReactElementM ViewEventHandler ()
portIOExpanded_ ref nl p = if p ^. Port.portId == InPortId Self then portSelf_ ref nl p else do
    let portId    = p ^. Port.portId
        portRef   = toAnyPortRef nl portId
        portType  = toString $ p ^. Port.valueType
        isInput   = isInPort portId
        num       = getPortNumber portId
        color     = convert $ p ^. Port.color
        py        = jsShow2 (lineHeight * fromIntegral (num + n))
        highlight = if isHighlighted p then ["hover"] else []
        classes   = if isInput then [ "port", "port--i", "port--i--" <> show (num + 1)] ++ highlight
                               else [ "port", "port--o", "port--o--" <> show (num + 1)] ++ highlight
        n         = if isInput then 1 else 0
    g_
        [ "className" $= Style.prefixFromList classes
        ] $ do
        text_
            [ "className" $= Style.prefixFromList [ "port__type", "noselect" ]
            , "key"       $= (jsShow portId <> "-type")
            , "y"         $= py
            , "dy"        $= "4px"
            , "dx"        $= (if isInput then "-16px" else "176px")
            ] $ elemString portType
        circle_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> jsShow num <> "-shape")
            , "fill"      $= color
            , "r"         $= jsShow2 3
            , "cy"        $= (py <> "px")
            ] mempty
        circle_
            ( handlers ref portRef ++
              [ "className" $= Style.prefix "port__select"
              , "key"       $= (jsShow portId <> jsShow num <> "-select")
              , "r"         $= jsShow2 (lineHeight/1.5)
              , "cy"        $= (py <> "px")
              ]
            ) mempty

portPhantom_ :: Ref App -> AnyPortRef -> ReactElementM ViewEventHandler ()
portPhantom_ ref portRef = do
    g_
        [ "className" $= Style.prefixFromList ["port"]
        ] $ do
        circle_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= "phantom-shape"
            , "r"         $= jsShow2 3
            , "fill"      $= "gray"
            , "cy"        $= "30px"
            ] mempty
        circle_
            ( handlers ref portRef ++
              [ "className" $= Style.prefix "port__select"
              , "key"       $= "phantom-select"
              , "r"         $= jsShow2 (lineHeight/1.5)
              , "cy"        $= "30px"
              ]
            ) mempty

portSidebar_ :: Bool -> ReactElementM ViewEventHandler ()
portSidebar_ isInput = do
    let classes = Style.prefixFromList [ "port-sidebar", if isInput then "port-sidebar--i" else "port-sidebar--o" ]
        key     = "portSidebar" <> if isInput then "Inputs" else "Outputs"
    div_
        [ "className" $= classes
        , "key"       $= key
        ] $
        svg_ [] $ do
            if isInput then do
                g_
                    [ "className" $= Style.prefixFromList [ "port", "port--self" ]
                    ] $ do
                    circle_
                        [ "className" $= Style.prefix "port__shape"
                        , "fill"      $= "orange"
                        , "r"         $= "5"
                        ] mempty
                    circle_
                        [ "className" $= Style.prefix "port__select"
                        , "r"         $= "13"
                        ] mempty
                g_
                    [ "className" $= Style.prefixFromList [ "port", "port--i" ]
                    ] $ do
                    circle_
                        [ "className" $= Style.prefix "port__shape"
                        , "fill"      $= "orange"
                        , "r"         $= "3"
                        , "cy"        $= "16"
                        ] mempty
                    circle_
                        [ "className" $= Style.prefix "port__select"
                        , "r"         $= "10.67"
                        , "cy"        $= "16"
                        ] mempty
            else g_
                    [ "className" $= Style.prefixFromList [ "port", "port--o" ]
                    ] $ do
                        circle_
                            [ "className" $= Style.prefix "port__shape"
                            , "fill"      $= "blue"
                            , "r"         $= "3"
                            ] mempty
                        circle_
                            [ "className" $= Style.prefix "port__select"
                            , "r"         $= "10.67"
                            ] mempty
