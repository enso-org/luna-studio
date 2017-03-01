{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Monad where

import           Data.Position                         (Position (Position), Vector2 (Vector2), x, y)
import           Empire.API.Data.TypeRep               (TypeRep)
import           Luna.Studio.Action.Geometry.Constants (gridSize)
import qualified Luna.Studio.Data.Color                as Color
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node          (Node)
import qualified Luna.Studio.React.Model.Node          as Node
import qualified Luna.Studio.React.View.Style          as Style
import           React.Flux                            as React


objName :: JSString
objName = "monad"

monadPadding :: Double
monadPadding = 2 * gridSize

nodeToMonadPoint :: Int -> Int -> Node -> Position
nodeToMonadPoint num allMonads node = Position (Vector2 x' y')
    where a  = if allMonads == 1 then 0 else (fromIntegral num - ((fromIntegral allMonads - 1) / 2)) * monadPadding
          x' =  node ^. Node.position ^. x
          y' = (node ^. Node.position ^. y) + a

monad :: ReactView (TypeRep, [Position])
monad = React.defineView objName $ \case
    (_ , []) -> mempty
    (tr, a ) -> do
        let start   = Position (Vector2 (-7000) ((head a) ^. y))
            end     = Position (Vector2   7000  ((last a) ^. y))
            a'      = start:a++[end]
            points  = fromString $ unwords $ map (\n -> show (n ^. x) <> "," <> show (n ^. y)) a'
        polyline_
            [ "className" $= Style.prefix "monad"
            , "points"    $= points
            , "stroke"    $= convert (Color.s .~ 0.28 $ Color.l .~ 0.140 $ Color.toHsl $ Color.fromType tr)
            ] mempty

monad_ :: Int -> (Int, (TypeRep, [Node])) -> ReactElementM ViewEventHandler ()
monad_ allMonads (num, (tr, nodes)) = React.viewWithSKey monad (jsShow num) (tr, map (nodeToMonadPoint num allMonads) nodes) mempty
