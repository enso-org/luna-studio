{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Port where

import           Empire.API.Data.Port              (InPort (Self), OutPort (All), PortId (InPortId, OutPortId), getPortNumber, isInPort)
import           Empire.API.Data.PortRef           (AnyPortRef)
import           Luna.Studio.Action.State.Model    (portAngleStart, portAngleStop)
import qualified Luna.Studio.Event.Mouse           as Mouse
import qualified Luna.Studio.Event.UI              as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Port      as Port
import           Luna.Studio.React.Model.App       (App)
import           Luna.Studio.React.Model.Constants (lineHeight, nodeRadius, nodeRadius')
import           Luna.Studio.React.Model.Port      (Port)
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


port :: ReactView (Ref App, Int, Bool, Port)
port = React.defineView name $ \(ref, numOfPorts, isOnly, p) ->
    case p ^. Port.portId of
        InPortId   Self          ->                portSelf_   ref p
        OutPortId  All           -> if isOnly then portSingle_ ref p
                                    else           portIO_     ref p numOfPorts
        _                        ->                portIO_     ref p numOfPorts

portExpanded :: ReactView (Ref App, Port)
portExpanded = React.defineView name $ \(ref, p) ->
    case p ^. Port.portId of
        InPortId   Self          -> portSelf_       ref p
        _                        -> portIOExpanded_ ref p

port_ :: Ref App -> Port -> Int -> Bool -> ReactElementM ViewEventHandler ()
port_ ref p numOfPorts isOnly =
    React.viewWithSKey port (jsShow $ p ^. Port.portId) (ref, numOfPorts, isOnly, p) mempty

portExpanded_ :: Ref App -> Port -> ReactElementM ViewEventHandler ()
portExpanded_ ref p =
    React.viewWithSKey portExpanded (jsShow $ p ^. Port.portId) (ref, p) mempty

handlers :: Ref App -> AnyPortRef -> [PropertyOrHandler [SomeStoreAction]]
handlers ref portRef = [ onMouseDown  $ handleMouseDown  ref portRef
                       , onMouseUp    $ handleMouseUp    ref portRef
                       , onClick      $ handleClick      ref portRef
                       , onMouseEnter $ handleMouseEnter ref portRef
                       , onMouseLeave $ handleMouseLeave ref portRef
                       ]

portSelf_ :: Ref App -> Port -> ReactElementM ViewEventHandler ()
portSelf_ ref p = do
    let portRef   = p ^. Port.portRef
        color     = convert $ p ^. Port.color
        portId    = p ^. Port.portId
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
                  [ "className"  $= Style.prefix "port__select"
                  , "key"        $= (jsShow portId <> "b")
                  , "r"          $= (fromString $ show $ r + lineHeight/2)
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
                [ "className"     $= Style.prefixFromList [ "port__select", "invisible" ]
                  , "key"         $= (jsShow portId <> "b")
                  , "fillOpacity" $= (fromString $ show (1 :: Int))
                  ] mempty

portSingle_ :: Ref App -> Port -> ReactElementM ViewEventHandler ()
portSingle_ ref p = do
    let portRef   = p ^. Port.portRef
        portId    = p ^. Port.portId
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

portIO_ :: Ref App -> Port -> Int -> ReactElementM ViewEventHandler ()
portIO_ ref p numOfPorts = do
    let portRef   = p ^. Port.portRef
        portId    = p ^. Port.portId
        isInput   = isInPort portId
        num       = getPortNumber portId
        color     = convert $ p ^. Port.color
        highlight = if p ^. Port.highlight then ["hover"] else []
        classes   = if isInput then [ "port", "port--i", "port--i--" <> show (num + 1)] ++ highlight
                               else [ "port", "port--o", "port--o--" <> show (num + 1)] ++ highlight
        svgFlag1  = if isInput then "1"  else "0"
        svgFlag2  = if isInput then "0"  else "1"
        mode      = if isInput then -1.0 else 1.0
        startPortArcX r = r * sin(portAngleStart num numOfPorts r * mode)
        startPortArcY r = r * cos(portAngleStart num numOfPorts r * mode)
        stopPortArcX  r = r * sin(portAngleStop  num numOfPorts r * mode)
        stopPortArcY  r = r * cos(portAngleStop  num numOfPorts r * mode)
        ax = jsShow2 . startPortArcX . (+) nodeRadius
        ay = jsShow2 . startPortArcY . (+) nodeRadius
        bx = jsShow2 . stopPortArcX  . (+) nodeRadius
        by = jsShow2 . stopPortArcY  . (+) nodeRadius
        cx = jsShow2 $ stopPortArcX  nodeRadius'
        cy = jsShow2 $ stopPortArcY  nodeRadius'
        dx = jsShow2 $ startPortArcX nodeRadius'
        dy = jsShow2 $ startPortArcY nodeRadius'
        r1 = jsShow2 . (+) nodeRadius
        r2 = jsShow2 nodeRadius'
        svgPath a = "M"  <> ax a <> " " <> ay a <>
                   " A " <> r1 a <> " " <> r1 a <> " 0 0 " <> svgFlag1 <> " " <> bx a <> " " <> by a <>
                   " L " <> cx   <> " " <> cy   <>
                   " A " <> r2   <> " " <> r2   <> " 0 0 " <> svgFlag2 <> " " <> dx   <> " " <> dy   <>
                   " Z"
    g_
        [ "className" $= Style.prefixFromList classes
        ] $ do
        path_
            [ "className" $= Style.prefix "port__shape"
            , "key"       $= (jsShow portId <> "a")
            , "fill"      $= color
            , "d"         $= svgPath 0
            ] mempty
        path_
            ( handlers ref portRef ++
              [ "className" $= Style.prefix "port__select"
              , "key"       $= (jsShow portId <> "b")
              , "d"         $= svgPath lineHeight
              ]
            ) mempty

portIOExpanded_ :: Ref App -> Port -> ReactElementM ViewEventHandler ()
portIOExpanded_ ref p = if p ^. Port.portId == InPortId Self then portSelf_ ref p else do
    let portRef   = p ^. Port.portRef
        portId    = p ^. Port.portId
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
            [ "className" $= Style.prefix "port__type"
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
