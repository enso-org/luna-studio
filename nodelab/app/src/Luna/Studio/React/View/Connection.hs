{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Connection where

import           Luna.Studio.Data.Vector            (Position, Vector2 (Vector2), x, y)
import           Luna.Studio.Prelude
import           React.Flux
import qualified React.Flux                         as React
import qualified Event.UI                           as UI
import           Luna.Studio.Data.Angle             (Angle)
import           Luna.Studio.Data.Color             (Color (Color))
import           Luna.Studio.Data.HSL               (color')
import           Luna.Studio.React.Model.Connection (Connection, CurrentConnection)
import qualified Luna.Studio.React.Model.Connection as Connection
import           Luna.Studio.React.Store            (Ref, dt)
import qualified Luna.Studio.React.Store            as Store
import           Luna.Studio.React.View.Global


name :: JSString
name = "connection-editor"


connection :: Ref Connection -> ReactView ()
connection connectionRef = React.defineControllerView
    name connectionRef $ \connectionStore () -> do
        let connection = connectionStore ^. dt
            src        = connection ^. Connection.from
            dst        = connection ^. Connection.to
            color      = connection ^. Connection.color
            num        = 0
            numOfPorts = 0
            num'       = 0
            numOfPorts'= 0
            isSingle   = True
            isSelf     = True
            srcXY      = connectionSrc src dst num  numOfPorts  isSingle
            dstXY      = connectionDst src dst num' numOfPorts' isSelf
        drawConnection_ srcXY dstXY color

connection_ :: Ref Connection -> ReactElementM ViewEventHandler ()
connection_ connectionRef = React.view (connection connectionRef) () mempty


currentConnection :: Ref CurrentConnection -> ReactView ()
currentConnection connectionRef = React.defineControllerView
    name connectionRef $ \connectionStore () -> do
        let connection = connectionStore ^. dt
            src        = connection ^. Connection.currentFrom
            dst        = connection ^. Connection.currentTo
            color      = connection ^. Connection.currentColor
            num        = 0
            numOfPorts = 0
            isSingle   = True
            srcXY      = connectionSrc src dst num numOfPorts isSingle
        drawConnection_ srcXY dst color

currentConnection_ :: Ref CurrentConnection -> ReactElementM ViewEventHandler ()
currentConnection_ connectionRef = React.view (currentConnection connectionRef) () mempty


drawConnection_ :: Position -> Position -> Int -> ReactElementM ViewEventHandler ()
drawConnection_ src dst color =
    let x1 = fromString $ showSvg $ src ^. x
        y1 = fromString $ showSvg $ src ^. y
        x2 = fromString $ showSvg $ dst ^. x
        y2 = fromString $ showSvg $ dst ^. y
        color = color' $ Color 5 --TODO[react]: Apply correct color
        width = fromString $ show connectionWidth
    in line_
        [ "x1"          $= x1
        , "y1"          $= y1
        , "x2"          $= x2
        , "y2"          $= y2
        , "stroke"      $= color
        , "strokeWidth" $= width
        ] mempty
