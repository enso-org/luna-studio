{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Connection where

import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude
import           React.Flux
import qualified React.Flux                         as React

import qualified Event.UI                           as UI
import           Luna.Studio.Data.Color             (Color (Color))
import           Luna.Studio.Data.HSL               (color')
import           Luna.Studio.React.Model.Connection (Connection, CurrentConnection)
import qualified Luna.Studio.React.Model.Connection as Connection
import           Luna.Studio.React.Store            (Ref, dt)
import qualified Luna.Studio.React.Store            as Store



name :: JSString
name = "connection-editor"

connectionWidth :: Double
connectionWidth = 3

connection :: Ref Connection -> ReactView ()
connection connectionRef = React.defineControllerView
    name connectionRef $ \connectionStore () -> do
        let connection = connectionStore ^. dt
            srcX       = connection ^. Connection.from . x
            srcY       = connection ^. Connection.from . y
            dstX       = connection ^. Connection.to . x
            dstY       = connection ^. Connection.to . y
            color      = connection ^. Connection.color
        drawConnection_ srcX srcY dstX dstY color

connection_ :: Ref Connection -> ReactElementM ViewEventHandler ()
connection_ connectionRef = React.view (connection connectionRef) () mempty


currentConnection :: Ref CurrentConnection -> ReactView ()
currentConnection connectionRef = React.defineControllerView
    name connectionRef $ \connectionStore () -> do
        let connection = connectionStore ^. dt
            srcX       = connection ^. Connection.currentFrom . x
            srcY       = connection ^. Connection.currentFrom . y
            dstX       = connection ^. Connection.currentTo . x
            dstY       = connection ^. Connection.currentTo . y
            color      = connection ^. Connection.currentColor
        drawConnection_ srcX srcY dstX dstY color

currentConnection_ :: Ref CurrentConnection -> ReactElementM ViewEventHandler ()
currentConnection_ connectionRef = React.view (currentConnection connectionRef) () mempty


drawConnection_ :: Double -> Double -> Double -> Double -> Int -> ReactElementM ViewEventHandler ()
drawConnection_ x1 y1 x2 y2 color = do
    let x1'   = fromString $ show x1
        y1'   = fromString $ show y1
        x2'   = fromString $ show x2
        y2'   = fromString $ show y2
        color = color' $ Color 5 --TODO[react]: Apply correct color
        width = fromString $ show connectionWidth
    line_
        [ "className"   $= "connection"
        , "x1"          $= x1'
        , "y1"          $= y1'
        , "x2"          $= x2'
        , "y2"          $= y2'
        , "stroke"      $= color
        , "strokeWidth" $= width
        ] mempty


nodeToNodeAngle :: Double -> Double -> Double -> Double -> Double
nodeToNodeAngle x1 y1 x2 y2 = atan $ (y1-y2) / (x1-x2) -- FIXME


{-- The simplest version:
    if     nodeToNodeAngle <= outputStart then outputAngleStart
    elseif nodeToNodeAngle >= outputStop then outputAngle
    else   nodeToNodeAngle
--}
