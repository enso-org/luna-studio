{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Connection where

import qualified Event.UI                           as UI
import           Luna.Studio.Data.Angle             (Angle)
import           Luna.Studio.Data.Color             (Color, toJSString)
import           Luna.Studio.Data.Vector            (Position, Vector2 (Vector2), x, y)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection (Connection, CurrentConnection)
import qualified Luna.Studio.React.Model.Connection as Connection
import           Luna.Studio.React.Store            (Ref, dt)
import qualified Luna.Studio.React.Store            as Store
import           Luna.Studio.React.View.Global
import           React.Flux
import qualified React.Flux                         as React


name :: JSString
name = "connection-editor"


connection :: Ref Connection -> ReactView ()
connection connectionRef = React.defineControllerView
    name connectionRef $ \connectionStore () -> do
        let connection = connectionStore ^. dt
            src        = connection ^. Connection.from
            dst        = connection ^. Connection.to
            color      = connection ^. Connection.color
        drawConnection_ src dst color

connection_ :: Ref Connection -> ReactElementM ViewEventHandler ()
connection_ connectionRef = React.view (connection connectionRef) () mempty


currentConnection :: Ref CurrentConnection -> ReactView ()
currentConnection connectionRef = React.defineControllerView
    name connectionRef $ \connectionStore () -> do
        let connection = connectionStore ^. dt
            src        = connection ^. Connection.currentFrom
            dst        = connection ^. Connection.currentTo
            color      = connection ^. Connection.currentColor
        drawConnection_ src dst color

currentConnection_ :: Ref CurrentConnection -> ReactElementM ViewEventHandler ()
currentConnection_ connectionRef = React.view (currentConnection connectionRef) () mempty


drawConnection_ :: Position -> Position -> Color -> ReactElementM ViewEventHandler ()
drawConnection_ src dst color = do
    let x1 = fromString $ showSvg $ src ^. x
        y1 = fromString $ showSvg $ src ^. y
        x2 = fromString $ showSvg $ dst ^. x
        y2 = fromString $ showSvg $ dst ^. y
        width = fromString $ show connectionWidth
    line_
        [ "x1"          $= x1
        , "y1"          $= y1
        , "x2"          $= x2
        , "y2"          $= y2
        , "stroke"      $= toJSString color
        , "strokeWidth" $= width
        ] mempty
