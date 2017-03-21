{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Port where

import           Empire.API.Data.PortRef           (AnyPortRef, toAnyPortRef)
import           Luna.Studio.Action.State.Model    (portAngleStart, portAngleStop)
import qualified Luna.Studio.Event.Mouse           as Mouse
import qualified Luna.Studio.Event.UI              as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Port      as Port
import           Luna.Studio.React.Model.App       (App)
import           Luna.Studio.React.Model.Constants (lineHeight, nodeRadius, nodeRadius')
import           Luna.Studio.React.Model.Node      (NodeId)
import           Luna.Studio.React.Model.Port      (InPort (Self), OutPort (All), Port, PortId (InPortId, OutPortId), getPortNumber,
                                                    isInPort)
import qualified Luna.Studio.React.Model.Port      as Port
import           Luna.Studio.React.Store           (Ref, dispatch)
import qualified Luna.Studio.React.View.Style      as Style
import           Numeric                           (showFFloat)
import           React.Flux                        hiding (view)
import qualified React.Flux                        as React

name :: JSString
name = "port"

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


port :: ReactView (Ref App, NodeId, Int, Bool, Port)
port = React.defineView name $ \(ref, nid, numOfPorts, isOnly, p) ->
    case p ^. Port.portId of
        InPortId   Self          ->                portSelf_   ref nid p
        OutPortId  All           -> if isOnly then portSingle_ ref nid p
                                    else           portIO_     ref nid p numOfPorts
        _                        ->                portIO_     ref nid p numOfPorts

portExpanded :: ReactView (Ref App, NodeId, Port)
portExpanded = React.defineView name $ \(ref, nid, p) ->
    case p ^. Port.portId of
        InPortId   Self          -> portSelf_       ref nid p
        _                        -> portIOExpanded_ ref nid p

port_ :: Ref App -> NodeId -> Port -> Int -> Bool -> ReactElementM ViewEventHandler ()
port_ ref nid p numOfPorts isOnly =
    React.viewWithSKey port (jsShow $ p ^. Port.portId) (ref, nid, numOfPorts, isOnly, p) mempty

portExpanded_ :: Ref App -> NodeId -> Port -> ReactElementM ViewEventHandler ()
portExpanded_ ref nid p =
    React.viewWithSKey portExpanded (jsShow $ p ^. Port.portId) (ref, nid, p) mempty

handlers :: Ref App -> AnyPortRef -> [PropertyOrHandler [SomeStoreAction]]
handlers ref portRef = [ onMouseDown  $ handleMouseDown  ref portRef
                       , onMouseUp    $ handleMouseUp    ref portRef
                       , onClick      $ handleClick      ref portRef
                       , onMouseEnter $ handleMouseEnter ref portRef
                       , onMouseLeave $ handleMouseLeave ref portRef
                       ]

portSelf_ :: Ref App -> NodeId -> Port -> ReactElementM ViewEventHandler ()
portSelf_ ref nid p = do
    let portId    = p ^. Port.portId
        portRef   = toAnyPortRef nid portId
        color     = convert $ p ^. Port.color
        highlight = if p ^. Port.highlight then ["hover"] else []
        visible   = p ^. Port.visible
        className = Style.prefixFromList $ [ "port", "port--self" ] ++ highlight
        r         = 5
    if visible then
        g_
            [ "className" $= className ] $ do
            circle_
                [ "className" $= Style.prefix "port__shape"
                , "key"       $= (jsShow portId <> "a")
                , "fill"      $= color
                , "r"         $= (fromString $ show r)
                ] mempty
            circle_
                ( handlers ref portRef ++
                  [ "className" $= Style.prefix "port__select"
                  , "key"       $= (jsShow portId <> "b")
                  , "r"         $= (fromString $ show $ r + lineHeight/2)
                  ]
                ) mempty
    else g_
            [ "className" $= className ] $ do
            circle_
                [ "className"   $= Style.prefixFromList [ "port__shape", "invisible" ]
                , "key"         $= (jsShow portId <> "a")
                , "fillOpacity" $= (fromString $ show (1 :: Int))
                ] mempty
            circle_
                [ "className"   $= Style.prefixFromList [ "port__select", "invisible" ]
                , "key"         $= (jsShow portId <> "b")
                , "fillOpacity" $= (fromString $ show (1 :: Int))
                ] mempty

portSingle_ :: Ref App -> NodeId -> Port -> ReactElementM ViewEventHandler ()
portSingle_ ref nid p = do
    let portId    = p ^. Port.portId
        portRef   = toAnyPortRef nid portId
        color     = convert $ p ^. Port.color
        highlight = if p ^. Port.highlight then ["hover"] else []
        className = Style.prefixFromList $ [ "port", "port--o--single" ] ++ highlight
        r1 :: Double -> JSString
        r1 = jsShow2 . (+) nodeRadius
        r2 = jsShow2 nodeRadius'
        svgPath :: Double -> Integer -> Integer -> JSString
        svgPath a b c = "M0 -" <> r1 a <> " A " <> r1 a <> " " <> r1 a <> " 0 0 " <> jsShow b <> " 0 "  <> r1 a <>
                       " L0 "  <> r2   <> " A " <> r2   <> " " <> r2   <> " 1 0 " <> jsShow c <> " 0 -" <> r2   <> " Z "
    g_ [ "className" $= className ] $ do
        path_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> "a" )
            , "fill"      $= color
            , "d"         $= (svgPath 0 0 1 <> svgPath 0 1 0)
            ] mempty
        path_
            ( handlers ref portRef ++
              [ "className" $= Style.prefix "port__select"
              , "key"       $= (jsShow portId <> "b")
              , "d"         $= (svgPath lineHeight 0 1 <> svgPath lineHeight 1 0)
              ]
            ) mempty

portIO_ :: Ref App -> NodeId -> Port -> Int -> ReactElementM ViewEventHandler ()
portIO_ ref nid p numOfPorts = do
    let portId    = p ^. Port.portId
        portRef   = toAnyPortRef nid portId
        isInput   = isInPort portId
        num       = getPortNumber portId
        color     = convert $ p ^. Port.color
        highlight = if p ^. Port.highlight then ["hover"] else []
        classes   = if isInput then [ "port", "port--i", "port--i--" <> show (num + 1) ] ++ highlight
                               else [ "port", "port--o", "port--o--" <> show (num + 1) ] ++ highlight
        svgFlag1  = if isInput then "1"  else "0"
        svgFlag2  = if isInput then "0"  else "1"
        mode      = if isInput then -1.0 else 1.0
--        n         = if isInput then 1 else 0
        adjust
            | numOfPorts == 1 = (-3.5)
            | numOfPorts == 2 =   4.5
            | numOfPorts == 3 =  12.5
            | otherwise       =  20.5
        portType  = toString $ p ^. Port.valueType
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
            , "y"         $= jsShow2 ((lineHeight * fromIntegral num) - adjust)
            , "x"         $= (if isInput then "-40" else "40")
            ] $ elemString portType
        path_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> "a")
            , "fill"      $= color
            , "d"         $= svgPath True 0
            ] mempty
        path_
            ( handlers ref portRef ++
              [ "className" $= Style.prefix "port__select"
              , "key"       $= (jsShow portId <> "b")
              , "d"         $= svgPath False lineHeight
              ]
            ) mempty

portIOExpanded_ :: Ref App -> NodeId -> Port -> ReactElementM ViewEventHandler ()
portIOExpanded_ ref nid p = if p ^. Port.portId == InPortId Self then portSelf_ ref nid p else do
    let portId    = p ^. Port.portId
        portRef   = toAnyPortRef nid portId
        portType  = toString $ p ^. Port.valueType
        isInput   = isInPort portId
        num       = getPortNumber portId
        color     = convert $ p ^. Port.color
        py        = jsShow2 (lineHeight * fromIntegral (num + n))
        highlight = if p ^. Port.highlight then ["hover"] else []
        classes   = if isInput then [ "port", "port--i", "port--i--" <> show (num + 1)] ++ highlight
                               else [ "port", "port--o", "port--o--" <> show (num + 1)] ++ highlight
        n         = if isInput then 1 else 0
    g_
        [ "className" $= Style.prefixFromList classes
        ] $ do
        text_
            [ "className" $= Style.prefixFromList [ "port__type", "noselect" ]
            , "y"         $= py
            , "dy"        $= "4"
            , "dx"        $= (if isInput then "-16" else "176")
            ] $ elemString portType
        circle_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> jsShow num <> "a")
            , "fill"      $= color
            , "r"         $= jsShow2 3
            , "cy"        $= py
            ] mempty
        circle_
            ( handlers ref portRef ++
              [ "className" $= Style.prefix "port__select"
              , "key"       $= (jsShow portId <> jsShow num <> "b")
              , "r"         $= jsShow2 (lineHeight/1.5)
              , "cy"        $= py
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
