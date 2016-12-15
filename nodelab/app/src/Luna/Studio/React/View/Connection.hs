{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Connection where

import           React.Flux
import qualified React.Flux             as React
import           Luna.Studio.Prelude

import qualified Event.UI               as UI
import           Luna.Studio.React.Store            (Ref, dt)
import qualified Luna.Studio.React.Store            as Store
import           Luna.Studio.React.Model.Connection (Connection)
import qualified Luna.Studio.React.Model.Connection as Connection
import           Luna.Studio.Data.Color             (Color(Color))
import           Luna.Studio.Data.HSL               (color')


name :: JSString
name = "connection-editor"


connection :: Ref Connection -> ReactView ()
connection connectionRef = React.defineControllerView
    name connectionRef $ \connectionStore () -> do
        div_ $ do
            elemString $ "connection: " <> show (connectionStore ^. dt)


connection_ :: Ref Connection -> ReactElementM ViewEventHandler ()
connection_ connectionRef = React.view (connection connectionRef) () mempty


drawConnection_ :: Float -> Float -> Float -> Float -> String -> ReactElementM ViewEventHandler ()
drawConnection_ x1 y1 x2 y2 color = do
    let x1    = fromString $ show x1
        y1    = fromString $ show y1
        x2    = fromString $ show x2
        y2    = fromString $ show y2
        color = color' $ Color 5
    line_
        [ "className"   $= "connection"
        , "x1"          $= x1
        , "y1"          $= y1
        , "x2"          $= x2
        , "y2"          $= y2
        , "stroke"      $= color
        , "strokeWidth" $= "3"
        ] mempty


nodeToNodeAngle :: Float -> Float -> Float -> Float -> Float
nodeToNodeAngle x1 y1 x2 y2 = atan $ (y1-y2) / (x1-x2) -- TODO

inputAngle1 :: Float
inputAngle1 = 1

inputAngle2 :: Float
inputAngle2 = 2

outputAngle1 :: Float
outputAngle1 = 1

outputAngle2 :: Float
outputAngle2 = 1

{-- The simplest version:
    if     nodeToNodeAngle <= outputAngle1 then outputAngle1
    elseif nodeToNodeAngle >= outputAngle2 then outputAngle2
    else   nodeToNodeAngle
--}
