{-# LANGUAGE TypeFamilies #-}
module Data.Position
    ( module Data.Position
    , module X
    )
where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Vector         as X
import           Luna.Studio.Prelude
-----------------------
-- === Position === ---
-----------------------

-- === Definition === --

newtype Position = Position (Vector2 Double) deriving (Eq, Show, Generic, Default, NFData, Num)
makeWrapped ''Position


-- === Instances === --

type instance VectorOf Position = Vector2 Double

instance Dim1      Position
instance Dim2      Position
instance FromJSON  Position
instance IsVector  Position
instance ToJSON    Position

instance IsList Position where
    type Item Position = Double
    fromList l = Position (fromList l)
    toList   p = [p ^. x, p ^. y]


-- === Functions === ---

--TODO: [Position] -> Position
averagePosition :: Position -> Position -> Position
averagePosition a b =
    let ax = (a ^. x + b ^. x) / 2
        ay = (a ^. y + b ^. y) / 2
    in Position (Vector2 ax ay)

move :: Vector2 Double -> Position -> Position
move vec pos = pos & vector +~ vec

rescale :: Position -> Double -> Position
rescale pos factor = pos & vector %~ flip scalarProduct factor

leftTopPoint :: [Position] -> Maybe Position
leftTopPoint []        = Nothing
leftTopPoint positions = Just $ Position (Vector2 (minimum $ view x <$> positions) (minimum $ view y <$> positions))

rightBottomPoint :: [Position] -> Maybe Position
rightBottomPoint []        = Nothing
rightBottomPoint positions = Just $ Position (Vector2 (maximum $ view x <$> positions) (maximum $ view y <$> positions))

minimumRectangle :: [Position] -> Maybe (Position, Position)
minimumRectangle positions = (,) <$> leftTopPoint positions <*> rightBottomPoint positions

distance :: Position -> Position -> Double
distance p0 p1 = magnitude (p0 ^. vector - p1 ^. vector)

distanceSquared :: Position -> Position -> Double
distanceSquared p0 p1 = lengthSquared (p0 ^. vector - p1 ^. vector)

-- TODO[react]: Possible solution to differ Mouse Position and Graph Position
-- makeClassy  ''Position
-- class HasPosition a where
--     position :: Lens' a Position
--
-- instance HasPosition Position where
--     position = id
--
-- data Screen
-- data Graph
-- data Node
-- data Vis
--
--
-- newtype Coord t = Coord Position deriving (Eq, Show, Generic, Default)
-- makeWrapped ''Coord
--
--
-- rebase :: Coord t -> Coord t'
-- rebase = rewrapped
--
-- instance HasPosition (Coord t) where
--     position = unwrap'
--
-- instance HasVector (Coord t) where
--     vector = position . vector
--
-- instance Dim1 (Coord t)
-- instance Dim2 (Coord t)
--
-- Coord Screen
-- Coord Graph
-- Coord Node


-----------------------------
-- === ScreenPosition === ---
-----------------------------

-- TODO[react]: Introduce sth else instead of simplest alias
type ScreenPosition = Position
