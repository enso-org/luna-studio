{-# LANGUAGE TypeFamilies #-}
module Luna.Studio.Data.Vector where

import           Control.DeepSeq     (NFData)
import           Data.Aeson          (ToJSON)
import           Luna.Studio.Prelude

--TODO[react]: Make this import possible
-- import           Control.Lens.Utils  (makePfxLenses)
import           Prologue            (wrapped')



--TODO[react]: Consider change Vector2 -> V2: https://hackage.haskell.org/package/linear-1.20.5/docs/Linear-V2.html

--------------------
-- === Vector === --
--------------------

type family VectorOf a

class IsVector a where
    vector :: Lens' a (VectorOf a)
    default vector :: (Wrapped a, Unwrapped a ~ VectorOf a) => Lens' a (VectorOf a)
    vector = wrapped'


-- === Dimensions === --

class Dim1 a where
    x :: Lens' a (Item a)
    default x :: (IsVector a, Item (VectorOf a) ~ Item a, Dim1 (VectorOf a)) => Lens' a (Item a)
    x = vector . x

class Dim1 a => Dim2 a where
    y :: Lens' a (Item a)
    default y :: (IsVector a, Item (VectorOf a) ~ Item a, Dim2 (VectorOf a)) => Lens' a (Item a)
    y = vector . y

class Dim2 a => Dim3 a where
    z :: Lens' a (Item a)
    default z :: (IsVector a, Item (VectorOf a) ~ Item a, Dim3 (VectorOf a)) => Lens' a (Item a)
    z = vector . z



---------------------
-- === Vector2 === --
---------------------

-- === Definition === --

data Vector2 a = Vector2 { _vector2_x, _vector2_y :: a } deriving (Eq, Show, Functor, Generic)
makeLenses ''Vector2


-- === Instances === --

instance Dim1 (Vector2 a) where x = vector2_x
instance Dim2 (Vector2 a) where y = vector2_y
instance ToJSON a => ToJSON (Vector2 a)
instance NFData a => NFData (Vector2 a)

instance Default a => Default (Vector2 a) where
    def = Vector2 def def

instance Num a => Num (Vector2 a) where
    (Vector2 x1 y1) + (Vector2 x2 y2) = Vector2 (x1 + x2) (y1 + y2)
    (Vector2 x1 y1) - (Vector2 x2 y2) = Vector2 (x1 - x2) (y1 - y2)
    (Vector2 x1 y1) * (Vector2 x2 y2) = Vector2 (x1 * x2) (y1 * y2)
    abs    (Vector2 x1 y1)            = Vector2 (abs    x1) (abs    y1)
    signum (Vector2 x1 y1)            = Vector2 (signum x1) (signum y1)
    fromInteger i                     = let val = fromInteger i in Vector2 val val

instance IsList (Vector2 a) where
    type Item (Vector2 a) = a
    fromList [x',y'] = Vector2 x' y'
    fromList _     = error "List must be of length 2 to create Vector2."
    toList   vec   = [vec ^. x, vec ^. y]

instance Applicative Vector2 where
    pure v                          = Vector2 v v
    (Vector2 f g) <*> (Vector2 x y) = Vector2 (f x) (g y)

instance Monoid a => Monoid (Vector2 a) where
    mempty                                    = Vector2 mempty mempty
    (Vector2 x1 y1) `mappend` (Vector2 x2 y2) = Vector2 (x1 `mappend` x2) (y1 `mappend` y2)


-- === Functions === --

lengthSquared :: Num a => Vector2 a -> a
lengthSquared (Vector2 x' y') = x' * x' + y' * y'

magnitude :: Floating a => Vector2 a -> a
magnitude = sqrt . lengthSquared

normalize :: Vector2 Double -> Vector2 Double
normalize (Vector2 x' y') = Vector2 (x' / len) (y' / len) where len = sqrt $ x' * x' + y' * y'

explode :: Vector2 Double -> Vector2 Double
explode (Vector2 x' y') = Vector2 (fact * x') (fact * y') where
    fact  = shift (\x' -> 1.0 / (x' ** 4)) lenSq
    lenSq = x' * x' + y' * y'

shift :: (Double -> Double) -> Double -> Double
shift f x' = if x' < shiftConst then 0.0
                                else f (x' - shiftConst)
    where shiftConst = 0.1

nudgeFromZero :: Double -> Double
nudgeFromZero v = (sign v) * (0.1 + (abs v)) where
    sign v = if v == 0.0 then 1 else signum v

negateSnd :: Num a => Vector2 a -> Vector2 a
negateSnd (Vector2 x y) = Vector2 x (-y)

fromTuple :: Num a => (a, a) -> Vector2 a
fromTuple (a, b) = Vector2 a b

toTuple :: Num a => Vector2 a -> (a, a)
toTuple (Vector2 a b) = (a, b)

minMax :: Ord a => Vector2 a -> Vector2 a -> Vector2 a
minMax (Vector2 a b) (Vector2 a' b') = Vector2 (min a a') (max b b')

maxMin :: Ord a => Vector2 a -> Vector2 a -> Vector2 a
maxMin (Vector2 a b) (Vector2 a' b') = Vector2 (max a a') (min b b')

scalarProduct :: Num a => Vector2 a -> a -> Vector2 a
scalarProduct vec k = vec & x *~ k & y *~ k



-----------------------
-- === Position === ---
-----------------------

-- === Definition === --

newtype Position = Position (Vector2 Double) deriving (Eq, Show, Generic, Default, NFData)
makeWrapped ''Position


-- === Instances === --

type instance VectorOf Position = Vector2 Double

instance IsVector Position
instance Dim1     Position
instance Dim2     Position
instance ToJSON   Position

instance IsList Position where
    type Item Position = Double
    fromList l = Position (fromList l)
    toList   p = [p ^. x, p ^. y]


-- === Functions === ---

move :: Position -> Vector2 Double -> Position
move pos vec = pos & vector +~ vec

rescale :: Position -> Double -> Position
rescale pos factor = pos & vector %~ (flip scalarProduct factor)

leftTopPoint :: [Position] -> Maybe (Position)
leftTopPoint []        = Nothing
leftTopPoint positions = Just $ Position (Vector2 (minimum $ view x <$> positions) (minimum $ view y <$> positions))

rightBottomPoint :: [Position] -> Maybe (Position)
rightBottomPoint []        = Nothing
rightBottomPoint positions = Just $ Position (Vector2 (maximum $ view x <$> positions) (maximum $ view y <$> positions))

minimumRectangle :: [Position] -> Maybe (Position, Position)
minimumRectangle positions = (,) <$> (leftTopPoint positions) <*> (rightBottomPoint positions)

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



-------------------
-- === Size === ---
-------------------

-- === Definition === --

newtype Size = Size (Vector2 Double) deriving (Eq, Show, Generic, Default)
makeWrapped ''Size


-- === Instances === --

type instance VectorOf Size = Vector2 Double

instance IsVector Size
instance Dim1 Size
instance Dim2 Size
instance ToJSON Size

instance IsList Size where
    type Item Size = Double
    fromList l = Size (fromList l)
    toList   p = [p ^. x, p ^. y]



-----------------------------
-- === ScreenPosition === ---
-----------------------------

-- TODO[react]: Introduce sth else instead of simplest alias
type ScreenPosition = Position
