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
import           Luna.Studio.React.Model.App        (App)
import           Luna.Studio.React.Model.Port       (Port (..))
import qualified Luna.Studio.React.Model.Port       as Port
import           Luna.Studio.React.Store            (Ref, dispatch)
import           Numeric                            (showFFloat)
import           React.Flux                         hiding (view)
import qualified React.Flux                         as React


name :: JSString
name = "port"

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

port :: ReactView (Ref App, Int, Bool, Port)
port = React.defineView name $ \(ref, numOfPorts, isOnly, p) ->
    case p ^. Port.portId of
        InPortId   Self          ->                portSelf_   ref p
        OutPortId  All           -> if isOnly then portSingle_ ref p
                                    else           portIO_     ref p 0 numOfPorts False
        InPortId  (Arg        i) ->                portIO_     ref p i numOfPorts True
        OutPortId (Projection i) ->                portIO_     ref p i numOfPorts False

portExpanded :: ReactView (Ref App, Port)
portExpanded = React.defineView name $ \(ref, p) ->
    case p ^. Port.portId of
        InPortId   Self          -> portSelf_       ref p
        OutPortId  All           -> portIOExpanded_ ref p 0 False
        InPortId  (Arg        i) -> portIOExpanded_ ref p i True
        OutPortId (Projection i) -> portIOExpanded_ ref p i False

port_ :: Ref App -> Port -> Int -> Bool -> ReactElementM ViewEventHandler ()
port_ ref p numOfPorts isOnly =
    React.viewWithSKey port (jsShow $ p ^. Port.portId) (ref, numOfPorts, isOnly, p) mempty where

portExpanded_ :: Ref App -> Port -> ReactElementM ViewEventHandler ()
portExpanded_ ref p =
    React.viewWithSKey portExpanded (jsShow $ p ^. Port.portId) (ref, p) mempty

portSelf_ :: Ref App -> Port -> ReactElementM ViewEventHandler ()
portSelf_ ref port = do
    let portRef = port ^. Port.portRef
        color   = toJSString $ port ^. Port.color
        portId  = port ^. Port.portId
    g_
        [ "className" $= "luna-port luna-port--self" ] $ do
        circle_
            [ "className" $= "luna-port__shape"
            , "key"       $= (jsShow portId <> "a")
            , "fill"      $= color
            ] mempty
        circle_
            [ onMouseDown $ handleMouseDown ref portRef
            , onMouseUp   $ handleMouseUp   ref portRef
            , onClick     $ handleClick     ref portRef
            , "className" $= "luna-port__select"
            , "key"       $= (jsShow portId <> "b")
            ] mempty

portSingle_ :: Ref App -> Port -> ReactElementM ViewEventHandler ()
portSingle_ ref port = do
    let portRef = port ^. Port.portRef
        portId  = port ^. Port.portId
        color   = toJSString $ port ^. Port.color
        r1 = jsShow2 . (+) nodeRadius
        r2 = jsShow2 . (-) nodeRadius'
        svgPath a b c = "M0 -" <> r1 a <> " A " <> r1 a <> " " <> r1 a <> " 0 0 " <> jsShow b <> " 0 "  <> r1 a <>
                        " L0 "  <> r2 a <> " A " <> r2 a <> " " <> r2 a <> " 1 0 " <> jsShow c <> " 0 -" <> r2 a <> " Z "
    g_ [ "className" $= "luna-port luna-port--o--single" ] $ do
        path_
            [ "className" $= "luna-port__shape"
            , "key"       $= (jsShow portId <> "a" )
            , "fill"      $= color
            , "d"         $= (svgPath 0 0 1 <> svgPath 0 1 0)
            ] mempty
        path_
            [ onMouseDown $ handleMouseDown ref portRef
            , onMouseUp   $ handleMouseUp   ref portRef
            , onClick     $ handleClick     ref portRef
            , "className" $= "luna-port__select"
            , "key"       $= (jsShow portId <> "b")
            , "d"         $= (svgPath 3 0 1 <> svgPath 3 1 0)
            ] mempty

portIO_ :: Ref App -> Port -> Int -> Int -> Bool -> ReactElementM ViewEventHandler ()
portIO_ ref port num numOfPorts isInput = do
    let portRef = port ^. Port.portRef
        portId  = port ^. Port.portId
        color   = toJSString $ port ^. Port.color
        classes  = if isInput then "luna-port luna-port--i luna-port--i--" else "luna-port luna-port--o luna-port--o--"
        svgFlag1 = if isInput then "1"  else "0"
        svgFlag2 = if isInput then "0"  else "1"
        mod      = if isInput then -1.0 else 1.0
        startPortArcX r = r * sin(portAngleStart num numOfPorts r * mod)
        startPortArcY r = r * cos(portAngleStart num numOfPorts r * mod)
        stopPortArcX  r = r * sin(portAngleStop  num numOfPorts r * mod)
        stopPortArcY  r = r * cos(portAngleStop  num numOfPorts r * mod)
        ax = jsShow2 . startPortArcX . (+) nodeRadius
        ay = jsShow2 . startPortArcY . (+) nodeRadius
        bx = jsShow2 . stopPortArcX  . (+) nodeRadius
        by = jsShow2 . stopPortArcY  . (+) nodeRadius
        cx = jsShow2 . stopPortArcX  . (-) nodeRadius'
        cy = jsShow2 . stopPortArcY  . (-) nodeRadius'
        dx = jsShow2 . startPortArcX . (-) nodeRadius'
        dy = jsShow2 . startPortArcY . (-) nodeRadius'
        r1 = jsShow2 . (+) nodeRadius
        r2 = jsShow2 . (-) nodeRadius'
        svgPath a = "M"  <> ax a <> " " <> ay a <>
                    " A " <> r1 a <> " " <> r1 a <> " 1 0 " <> svgFlag1 <> " " <> bx a <> " " <> by a <>
                    " L " <> cx a <> " " <> cy a <>
                    " A " <> r2 a <> " " <> r2 a <> " 1 0 " <> svgFlag2 <> " " <> dx a <> " " <> dy a <>
                    " Z"
    g_
        [ "className" $= convert (classes <> show (num+1)) ] $ do
        path_
            [ "className" $= "luna-port__shape"
            , "key"       $= (jsShow portId <> "a")
            , "fill"      $= color
            , "d"         $= svgPath 0
            ] mempty
        path_
            [ onMouseDown $ handleMouseDown ref portRef
            , onMouseUp   $ handleMouseUp   ref portRef
            , onClick     $ handleClick     ref portRef
            , "className" $= "luna-port__select"
            , "key"       $= (jsShow portId <> "b")
            , "d"         $= svgPath 3
            ] mempty

portIOExpanded_ :: Ref App -> Port -> Int -> Bool -> ReactElementM ViewEventHandler ()
portIOExpanded_ ref port num isInput = do
    let portRef = port ^. Port.portRef
        portId  = port ^. Port.portId
        color   = toJSString $ port ^. Port.color
        classes = if isInput then "luna-port luna-port--i luna-port--i--" else "luna-port luna-port--o luna-port--o--"
        n       = if isInput then 1 else 0
        r       = jsShow2 . (+3)
    g_
        [ "className" $= convert (classes <> show (num + 1)) ] $ do
        circle_
            [ "className" $= "luna-port__shape"
            , "key"       $= (jsShow portId <> jsShow num <> "a")
            , "fill"      $= color
            , "r"         $= r 0
            , "cy"        $= jsShow2 (lineHeight * fromIntegral (num + n) )
            ] mempty
        circle_
            [ onMouseDown $ handleMouseDown ref portRef
            , onMouseUp   $ handleMouseUp   ref portRef
            , onClick     $ handleClick     ref portRef
            , "className" $= "luna-port__select"
            , "key"       $= (jsShow portId <> jsShow num <> "b")
            , "r"         $= r 3
            , "cy"        $= jsShow2 (lineHeight * fromIntegral (num + n) )
            ] mempty
