{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Monad where

import           Data.Position                   (Position(Position), Vector2(Vector2), x, y, getY)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node    (Node)
import qualified Luna.Studio.React.Model.Node    as Node
import           React.Flux


objName :: JSString
objName = "monad"

monadPadding :: Double
monadPadding = 30

nodeToMonadPoint :: Node -> Position
nodeToMonadPoint node = Position (Vector2 x' y')
    where num       = 2
          allMonads = 3
          a  = if allMonads == 1 then 0 else (num - 1 - ((allMonads - 1) / 2)) * monadPadding
          x' =  node ^. Node.position ^. x
          y' = (node ^. Node.position ^. y) + a

monadPolyline_ :: [Position] -> ReactElementM ViewEventHandler ()
monadPolyline_ a = do
    let key     = "monad"
        classes = "luna-monad"
        start   = Position (Vector2 (-7000) 0) -- FIXME getY $ head a
        end     = Position (Vector2   7000  0) -- FIXME getY $ last a
        b       = start:a++[end]
        points  = fromString $ unwords $ (\n -> show (n ^. x) <> "," <> show (n ^. y)) <$> b
    polyline_
        [ "key"       $= key
        , "className" $= classes
        , "points"    $= points
        ] mempty
