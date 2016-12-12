{-# LANGUAGE OverloadedStrings #-}
module React.View.Port where

import qualified Event.UI          as UI
import           React.Flux
import qualified React.Flux        as React
import           React.Store       (Ref, dispatch)
import           React.Store.Node   (Node)
import qualified React.Store.Node   as Node
import           Utils.PreludePlus
import           Object.Widget.Port (Port)
import qualified Object.Widget.Port as Port
import qualified Data.JSString.Text as JS
import qualified Numeric as Numeric


showF :: Float -> String
showF a = Numeric.showFFloat (Just 1) a ""


name :: JSString
name = "port"


port :: Ref Node -> ReactView Port
port ref = React.defineView name $ \port -> do
    portIO_


port_ :: Ref Node -> Port -> ReactElementM ViewEventHandler ()
port_ ref p = React.view (port ref) p mempty


portIO_ :: ReactElementM ViewEventHandler ()
portIO_ = do

    let color  = "#8ABEB7"
        inputs = 3 :: Int
        number = 1 :: Int

        line = 3 :: Float

        r1   = 20 :: Float
        r2   = r1 - line

        gap  = 0.15 :: Float
        gap' = gap * (r1/r2)

        t   = pi / fromIntegral inputs
        t1  = fromIntegral number * t - pi - t + gap/2
        t2  = fromIntegral number * t - pi - gap/2
        t1' = fromIntegral number * t - pi - t + gap'/2
        t2' = fromIntegral number * t - pi - gap'/2

        ax = showF $ r1 * sin(t1) + r1
        ay = showF $ r1 * cos(t1) + r1

        bx = showF $ r1 * sin(t2) + r1
        by = showF $ r1 * cos(t2) + r1

        cx = showF $ r2 * sin(t2') + r1
        cy = showF $ r2 * cos(t2') + r1

        dx = showF $ r2 * sin(t1') + r1
        dy = showF $ r2 * cos(t1') + r1
                                                                                                  -- v -- IO flag
        svgPath = fromString $ "M" <> ax <> " " <> ay <> " A " <> show r1 <> " " <> show r1 <> " 1 0 0 " <> bx <> " " <> by <>
                              " L" <> cx <> " " <> cy <> " A " <> show r2 <> " " <> show r2 <> " 1 0 1 " <> dx <> " " <> dy <>
                              " L" <> ax <> " " <> ay

    path_
        [ "className" $= (fromString $ "port port--i port--i--" <> show number)
        , "fill"      $= color
        , "stroke"    $= color
        , "d"         $= svgPath
        ] mempty


portSelf_ :: ReactElementM ViewEventHandler ()
portSelf_ = do
    let color = "#8ABEB7"
    circle_
        [ "className" $= "port port--self"
        , "fill"      $= color
        , "stroke"    $= color
        ] mempty
