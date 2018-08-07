module LunaStudio.Data.Position
    ( module LunaStudio.Data.Position
    , vector
    , x
    , y
    ) where

import           Control.Lens            ((+~), makeWrapped)
import           Data.Aeson.Types        (FromJSON, ToJSON)
import           Data.Binary             (Binary)
import           Foreign.Storable        (Storable)
import           LunaStudio.Data.Vector2 hiding (fromTuple, toTuple)
import           Prologue


-----------------------
-- === Position === ---
-----------------------

-- === Definition === --

newtype Position = Position
    { fromPosition :: Vector2 Double
    } deriving (Eq, Generic, Num, Ord, Show, Storable)

makeWrapped ''Position


-- === Instances === --

type instance VectorOf Position = Vector2 Double

instance Dim1     Position
instance Dim2     Position
instance IsVector Position
instance Binary   Position
instance Default  Position
instance NFData   Position
instance FromJSON Position
instance ToJSON   Position

type instance Item Position = Double
instance Convertible Position [Double] where convert = toList . view vector
instance Convertible [Double] Position where convert = Position . fromList

-- === Functions === ---

--TODO: [Position] -> Position
averagePosition :: Position -> Position -> Position
averagePosition a b =
    let ax = (a ^. x + b ^. x) / 2
        ay = (a ^. y + b ^. y) / 2
    in Position (Vector2 ax ay)

-- TODO: -- move :: IsVector a => Vector2 Double -> a -> a
move :: Vector2 Double -> Position -> Position
move vec pos = pos & vector +~ vec

rescale :: Position -> Double -> Position
rescale pos factor = pos & vector %~ flip scalarProduct factor

leftTopPoint :: [Position] -> Maybe Position
leftTopPoint []        = Nothing
leftTopPoint positions = Just . Position $ Vector2
    (unsafeMinimum $ view x <$> positions)
    (unsafeMinimum $ view y <$> positions)

rightBottomPoint :: [Position] -> Maybe Position
rightBottomPoint []        = Nothing
rightBottomPoint positions = Just . Position $ Vector2
    (unsafeMaximum $ view x <$> positions)
    (unsafeMaximum $ view y <$> positions)

minimumRectangle :: [Position] -> Maybe (Position, Position)
minimumRectangle positions = (,)
    <$> leftTopPoint positions
    <*> rightBottomPoint positions

distance :: Position -> Position -> Double
distance p0 p1 = magnitude (p0 ^. vector - p1 ^. vector)

distanceSquared :: Position -> Position -> Double
distanceSquared p0 p1 = lengthSquared (p0 ^. vector - p1 ^. vector)

fromTuple :: (Double, Double) -> Position
fromTuple = uncurry fromDoubles

toTuple :: Position -> (Double, Double)
toTuple (Position (Vector2 x' y')) = (x', y')

fromDoubles :: Double -> Double -> Position
fromDoubles = Position .: Vector2

onTuple :: (Position -> Position) -> (Double, Double) -> (Double, Double)
onTuple action = toTuple . action . fromTuple

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
