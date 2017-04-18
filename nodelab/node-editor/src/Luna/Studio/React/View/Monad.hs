{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Monad where

import           Data.Position                               (Position, fromDoubles, x, y)
import           Empire.API.Data.TypeRep                     (TypeRep)
import qualified Luna.Studio.Data.Color                      as Color
import           Luna.Prelude
import           Luna.Studio.React.Model.Constants           (gridSize)
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpressionNode, position)
import qualified Luna.Studio.React.View.Style                as Style
import           React.Flux                                  as React


objName :: JSString
objName = "monad"

monadPadding :: Double
monadPadding = 2 * gridSize

nodeToMonadPoint :: Int -> Int -> ExpressionNode -> Position
nodeToMonadPoint num allMonads node = fromDoubles x' y'
    where a  = if allMonads == 1 then 0
               else (fromIntegral num - ((fromIntegral allMonads - 1) / 2)) * monadPadding
          x' = node ^. position ^. x
          y' = node ^. position ^. y + a

monad :: ReactView (TypeRep, [Position])
monad = React.defineView objName $ \case
    (_ , []) -> mempty
    (tr, a ) -> do
        let start   = fromDoubles (-7000) $ (head a) ^. y
            end     = fromDoubles   7000  $ (last a) ^. y
            a'      = start : a ++ [end]
            points  = fromString $ unwords $ map (\n -> show (n ^. x) <> "," <> show (n ^. y)) a'
        polyline_
            [ "className" $= Style.prefix "monad"
            , "points"    $= points
            , "stroke"    $= convert (Color.buildLCH $ Color.fromType tr)
            ] mempty

monad_ :: Int -> (Int, (TypeRep, [ExpressionNode])) -> ReactElementM ViewEventHandler ()
monad_ allMonads (num, (tr, nodes)) = React.viewWithSKey monad (jsShow num) (tr, map (nodeToMonadPoint num allMonads) nodes) mempty

monads_ :: [(TypeRep, [ExpressionNode])] -> ReactElementM ViewEventHandler ()
monads_ monads = forKeyed_ monads $ monad_ (length monads)
