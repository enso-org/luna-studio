module Luna.Studio.Data.Vector where

import           Data.Aeson          (ToJSON)
import           Luna.Studio.Prelude

type Position = Vector2 Double

data Vector2 a = Vector2 { _x :: a
                         , _y :: a
                         } deriving (Eq, Show, Functor, Generic)

makeLenses ''Vector2

instance ToJSON a => ToJSON (Vector2 a)

instance Default a => Default (Vector2 a) where
    def = Vector2 def def

instance Num a => Num (Vector2 a) where
    (Vector2 x1 y1) + (Vector2 x2 y2) = Vector2 (x1 + x2) (y1 + y2)
    (Vector2 x1 y1) - (Vector2 x2 y2) = Vector2 (x1 - x2) (y1 - y2)
    (Vector2 x1 y1) * (Vector2 x2 y2) = Vector2 (x1 * x2) (y1 * y2)
    abs    (Vector2 x1 y1)            = Vector2 (abs    x1) (abs    y1)
    signum (Vector2 x1 y1)            = Vector2 (signum x1) (signum y1)
    fromInteger i                     = let val = fromInteger i in Vector2 val val

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

instance Applicative Vector2 where
    pure v                          = Vector2 v v
    (Vector2 f g) <*> (Vector2 x y) = Vector2 (f x) (g y)

instance Monoid a => Monoid (Vector2 a) where
    mempty                                    = Vector2 mempty mempty
    (Vector2 x1 y1) `mappend` (Vector2 x2 y2) = Vector2 (x1 `mappend` x2) (y1 `mappend` y2)

negateSnd :: Num a => Vector2 a -> Vector2 a
negateSnd (Vector2 x y) = Vector2 x (-y)

fromTuple :: Num a => (a, a) -> Vector2 a
fromTuple (a, b) = Vector2 a b

toTuple :: Num a => Vector2 a -> (a, a)
toTuple (Vector2 a b) = (a, b)
