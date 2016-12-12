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


data Portkind a b = Input  Int Int
                  | Output Int Int
                  | Self
                  deriving (Eq, Ord)


showF :: Float -> String
showF a = Numeric.showFFloat (Just 1) a ""


name :: JSString
name = "port"


port :: Ref Node -> ReactView Port
port ref = React.defineView name $ \port -> do
    drawPort_ (Output 1 3)


port_ :: Ref Node -> Port -> ReactElementM ViewEventHandler ()
port_ ref p = React.view (port ref) p mempty


drawPort_ :: Portkind a b -> ReactElementM ViewEventHandler ()
drawPort_ Self = let color = "#8ABEB7" in
    circle_
        [ "className" $= "port port--self"
        , "fill"      $= color
        , "stroke"    $= color
        ] mempty
drawPort_ (Input  a b) = drawPortIO_ a b   1  "0" "1"
drawPort_ (Output a b) = drawPortIO_ a b (-1) "1" "0"


drawPortIO_ :: Int -> Int -> Float -> String -> String -> ReactElementM ViewEventHandler ()
drawPortIO_ number inputs mod1 mod2 mod3 = do
    let color = "#8ABEB7"
        r1    = 20 :: Float
        line  = 3 :: Float
        gap   = 0.15 :: Float
        r2   = r1 - line
        gap' = gap * (r1/r2)

        t   = pi / fromIntegral inputs
        t1  = fromIntegral number * t - pi - t + gap/2
        t2  = fromIntegral number * t - pi - gap/2
        t1' = fromIntegral number * t - pi - t + gap'/2
        t2' = fromIntegral number * t - pi - gap'/2

        ax = showF $ r1 * sin(t1 * mod1) + r1
        ay = showF $ r1 * cos(t1 * mod1) + r1

        bx = showF $ r1 * sin(t2 * mod1) + r1
        by = showF $ r1 * cos(t2 * mod1) + r1

        cx = showF $ r2 * sin(t2' * mod1) + r1
        cy = showF $ r2 * cos(t2' * mod1) + r1

        dx = showF $ r2 * sin(t1' * mod1) + r1
        dy = showF $ r2 * cos(t1' * mod1) + r1

        svgPath = fromString $ "M" <> ax <> " " <> ay <> " A " <> show r1 <> " " <> show r1 <> " 1 0 " <> mod2 <> " " <> bx <> " " <> by <>
                              " L" <> cx <> " " <> cy <> " A " <> show r2 <> " " <> show r2 <> " 1 0 " <> mod3 <> " " <> dx <> " " <> dy <>
                              " L" <> ax <> " " <> ay

    path_
        [ "className" $= (fromString $ "port port--i port--i--" <> show number)
        , "fill"      $= color
        , "stroke"    $= color
        , "d"         $= svgPath
        ] mempty
