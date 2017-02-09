{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Port where

import           Empire.API.Data.Port               (InPort (..), OutPort (..), PortId (..))
import           Empire.API.Data.PortRef            (AnyPortRef)
import           Luna.Studio.Action.Geometry        (lineHeight, nodeRadius, nodeRadius', portAngleStart, portAngleStop)
import           Luna.Studio.Data.Color             (toJSString)
import qualified Luna.Studio.Event.Mouse            as Mouse
import qualified Luna.Studio.Event.UI               as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Connection as Connection
import qualified Luna.Studio.React.Event.Port       as Port
import           Luna.Studio.React.Model.App        (App)
import           Luna.Studio.React.Model.Port       (Port (..))
import qualified Luna.Studio.React.Model.Port       as Port
import           Luna.Studio.React.Store            (Ref, dispatch)
import           Numeric                            (showFFloat)
import           React.Flux                         hiding (view)
import qualified React.Flux                         as React


name :: JSString
name = "port"

selectAreaWidth :: Double
selectAreaWidth = 8

jsShow2 :: Double -> JSString
jsShow2 a = convert $ showFFloat (Just 2) a "" -- limit Double to two decimal numbers

handleMouseDown :: Ref App -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseDown ref portRef e m =
    if Mouse.withoutMods m Mouse.leftButton then
        stopPropagation e : dispatch ref (UI.ConnectionEvent $ Connection.StartConnection m portRef)
    else []

handleClick :: Ref App -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleClick ref portRef e m =
    if Mouse.withoutMods m Mouse.leftButton then
        stopPropagation e : dispatch ref (UI.ConnectionEvent $ Connection.Click m portRef)
    else []

handleMouseUp :: Ref App -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseUp ref portRef _e m = dispatch ref (UI.ConnectionEvent $ Connection.EndConnection m portRef)

handleMouseEnter :: Ref App -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseEnter ref portRef _e _m = dispatch ref (UI.PortEvent $ Port.MouseEnter portRef)

handleMouseLeave :: Ref App -> AnyPortRef -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseLeave ref portRef _e _m = dispatch ref (UI.PortEvent $ Port.MouseLeave portRef)


port :: ReactView (Ref App, Int, Bool, Port)
port = React.defineView name $ \(ref, numOfPorts, isOnly, p) ->
    case p ^. Port.portId of
        InPortId   Self          ->                portSelf_   ref p True
        OutPortId  All           -> if isOnly then portSingle_ ref p
                                    else           portIO_     ref p 0 numOfPorts False
        InPortId  (Arg        i) ->                portIO_     ref p i numOfPorts True
        OutPortId (Projection i) ->                portIO_     ref p i numOfPorts False

portExpanded :: ReactView (Ref App, Port)
portExpanded = React.defineView name $ \(ref, p) ->
    case p ^. Port.portId of
        InPortId   Self          -> portSelf_       ref p False
        OutPortId  All           -> portIOExpanded_ ref p 0 False
        InPortId  (Arg        i) -> portIOExpanded_ ref p i True
        OutPortId (Projection i) -> portIOExpanded_ ref p i False

port_ :: Ref App -> Port -> Int -> Bool -> ReactElementM ViewEventHandler ()
port_ ref p numOfPorts isOnly =
    React.viewWithSKey port (jsShow $ p ^. Port.portId) (ref, numOfPorts, isOnly, p) mempty

portExpanded_ :: Ref App -> Port -> ReactElementM ViewEventHandler ()
portExpanded_ ref p =
    React.viewWithSKey portExpanded (jsShow $ p ^. Port.portId) (ref, p) mempty

handlers :: Ref App -> AnyPortRef -> Bool -> [PropertyOrHandler [SomeStoreAction]]
handlers ref portRef isCollapsedSelf = (if isCollapsedSelf then id else ((onMouseDown $ handleMouseDown ref portRef):))
    [ onClick      $ handleClick      ref portRef
    , onMouseUp    $ handleMouseUp    ref portRef
    , onMouseEnter $ handleMouseEnter ref portRef
    , onMouseLeave $ handleMouseLeave ref portRef
    ]

portSelf_ :: Ref App -> Port -> Bool -> ReactElementM ViewEventHandler ()
portSelf_ ref p isNodeCollapsed = do
    let portRef   = p ^. Port.portRef
        color     = toJSString $ p ^. Port.color
        portId    = p ^. Port.portId
        highlight = if p ^. Port.highlight then " luna-hover" else ""
        className = fromString $ "luna-port luna-port--self" <> highlight
        r         = 5
    g_
        [ "className" $= className ] $ do
        circle_
            [ "className" $= "luna-port__shape"
            , "key"       $= (jsShow portId <> "a")
            , "fill"      $= color
            , "r"         $= (fromString $ show r)
            ] mempty
        circle_
            ( handlers ref portRef isNodeCollapsed ++
              [ "className"  $= "luna-port__select"
              , "key"        $= (jsShow portId <> "b")
              , "r"          $= (fromString $ show $ r + selectAreaWidth)
              ]
            ) mempty

portSingle_ :: Ref App -> Port -> ReactElementM ViewEventHandler ()
portSingle_ ref p = do
    let portRef   = p ^. Port.portRef
        portId    = p ^. Port.portId
        color     = toJSString $ p ^. Port.color
        highlight = if p ^. Port.highlight then " luna-hover" else ""
        className = fromString $ "luna-port luna-port--o--single" <> highlight
        r1 :: Double -> JSString
        r1 = jsShow2 . (+) nodeRadius
        r2 = jsShow2 nodeRadius'
        svgPath :: Double -> Integer -> Integer -> JSString
        svgPath a b c = "M0 -" <> r1 a <> " A " <> r1 a <> " " <> r1 a <> " 0 0 " <> jsShow b <> " 0 "  <> r1 a <>
                        " L0 " <> r2   <> " A " <> r2   <> " " <> r2   <> " 1 0 " <> jsShow c <> " 0 -" <> r2   <> " Z "
    g_ [ "className" $= className ] $ do
        path_
            [ "className" $= "luna-port__shape"
            , "key"       $= (jsShow portId <> "a" )
            , "fill"      $= color
            , "d"         $= (svgPath 0 0 1 <> svgPath 0 1 0)
            ] mempty
        path_
            ( handlers ref portRef False ++
              [ "className" $= "luna-port__select"
              , "key"       $= (jsShow portId <> "b")
              , "d"         $= (svgPath selectAreaWidth 0 1 <> svgPath selectAreaWidth 1 0)
              ]
            ) mempty

portIO_ :: Ref App -> Port -> Int -> Int -> Bool -> ReactElementM ViewEventHandler ()
portIO_ ref p num numOfPorts isInput = do
    let portRef   = p ^. Port.portRef
        portId    = p ^. Port.portId
        color     = toJSString $ p ^. Port.color
        highlight = if p ^. Port.highlight then " luna-hover" else ""
        classes   = if isInput then "luna-port luna-port--i luna-port--i--" else "luna-port luna-port--o luna-port--o--"
        className = fromString $ classes <> show (num+1) <> highlight
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
        [ "className" $= className ] $ do
        path_
            [ "className" $= "luna-port__shape"
            , "key"       $= (jsShow portId <> "a")
            , "fill"      $= color
            , "d"         $= svgPath 0
            ] mempty
        path_
            ( handlers ref portRef False ++
              [ "className" $= "luna-port__select"
              , "key"       $= (jsShow portId <> "b")
              , "d"         $= svgPath selectAreaWidth
              ]
            ) mempty

portIOExpanded_ :: Ref App -> Port -> Int -> Bool -> ReactElementM ViewEventHandler ()
portIOExpanded_ ref p num isInput = do
    let portRef   = p ^. Port.portRef
        portId    = p ^. Port.portId
        color     = toJSString $ p ^. Port.color
        highlight = if p ^. Port.highlight then " luna-hover" else ""
        classes   = if isInput then "luna-port luna-port--i luna-port--i--" else "luna-port luna-port--o luna-port--o--"
        className = fromString $ classes <> show (num + 1) <> highlight
        n         = if isInput then 1 else 0
    g_
        [ "className" $= className ] $ do
        circle_
            [ "className" $= "luna-port__shape"
            , "key"       $= (jsShow portId <> jsShow num <> "a")
            , "fill"      $= color
            , "r"         $= jsShow2 3
            , "cy"        $= jsShow2 (lineHeight * fromIntegral (num + n) )
            ] mempty
        circle_
            ( handlers ref portRef False ++
              [ "className" $= "luna-port__select"
              , "key"       $= (jsShow portId <> jsShow num <> "b")
              , "r"         $= jsShow2 (lineHeight/1.5)
              , "cy"        $= jsShow2 (lineHeight * fromIntegral (num + n) )
              ]
            ) mempty
