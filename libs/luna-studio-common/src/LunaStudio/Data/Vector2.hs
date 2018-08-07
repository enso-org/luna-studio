module LunaStudio.Data.Vector2 where

import           Control.Lens        ((*~), Rewrapped)
import           Data.Aeson.Types    (FromJSON, ToJSON)
import           Data.Binary         (Binary)
import           Foreign.Ptr         (castPtr, plusPtr)
import           Foreign.Storable    (Storable(..))
import           Prologue

--TODO[react]: Consider change Vector2 -> V2: https://hackage.haskell.org/package/linear-1.20.5/docs/Linear-V2.html

--------------------
-- === Vector === --
--------------------

type family VectorOf a

class IsVector a where
    vector :: Lens' a (VectorOf a)
    default vector :: (Wrapped a, Unwrapped a ~ VectorOf a)
        => Lens' a (VectorOf a)
    vector = wrapped'


-- === Dimensions === --

class Dim1 a where
    x :: Lens' a (Item a)
    default x :: (IsVector a, Item (VectorOf a) ~ Item a, Dim1 (VectorOf a))
        => Lens' a (Item a)
    x = vector . x

class Dim1 a => Dim2 a where
    y :: Lens' a (Item a)
    default y :: (IsVector a, Item (VectorOf a) ~ Item a, Dim2 (VectorOf a))
        => Lens' a (Item a)
    y = vector . y

class Dim2 a => Dim3 a where
    z :: Lens' a (Item a)
    default z :: (IsVector a, Item (VectorOf a) ~ Item a, Dim3 (VectorOf a))
        => Lens' a (Item a)
    z = vector . z



---------------------
-- === Vector2 === --
---------------------

-- === Definition === --

data Vector2 a = Vector2
    { _vector2_x, _vector2_y :: a
    } deriving (Eq, Functor, Generic, Ord, Show)

makeLenses ''Vector2

instance Storable a => Storable (Vector2 a) where
    sizeOf _ = 2 * sizeOf (undefined :: a)
    alignment _ = alignment (undefined :: a)
    peek p = Vector2 <$> peek (castPtr p)
                     <*> peek (p `plusPtr` sizeOf (undefined :: a))
    poke p v = do
        poke (castPtr p) (v ^. vector2_x)
        poke (p `plusPtr` (sizeOf (undefined :: a))) (v ^. vector2_y)


-- === Instances === --

instance Binary a => Binary (Vector2 a)
instance Dim1 (Vector2 a) where x = vector2_x
instance Dim2 (Vector2 a) where y = vector2_y
instance NFData   a => NFData   (Vector2 a)
instance FromJSON a => FromJSON (Vector2 a)
instance ToJSON   a => ToJSON   (Vector2 a)

instance Default a => Default (Vector2 a) where
    def = Vector2 def def

instance Num a => Num (Vector2 a) where
    (Vector2 x1 y1) + (Vector2 x2 y2) = Vector2 (x1 + x2) (y1 + y2)
    (Vector2 x1 y1) - (Vector2 x2 y2) = Vector2 (x1 - x2) (y1 - y2)
    (Vector2 x1 y1) * (Vector2 x2 y2) = Vector2 (x1 * x2) (y1 * y2)
    abs    (Vector2 x1 y1)            = Vector2 (abs    x1) (abs    y1)
    signum (Vector2 x1 y1)            = Vector2 (signum x1) (signum y1)
    fromInteger i
        = let val = fromInteger i in Vector2 val val

type instance Item (Vector2 a) = a
instance Convertible (Vector2 a) [a] where convert vec = [vec ^. x, vec ^. y]
instance Convertible [a] (Vector2 a) where
    convert [x',y'] = Vector2 x' y'
    convert _       = error "List must be of length 2 to create Vector2."

instance Applicative Vector2 where
    pure v                            = Vector2 v v
    (Vector2 f g) <*> (Vector2 x' y') = Vector2 (f x') (g y')

instance Mempty a => Mempty (Vector2 a) where
    mempty = Vector2 mempty mempty

instance Semigroup a => Semigroup (Vector2 a) where
    (Vector2 x1 y1) <> (Vector2 x2 y2) = Vector2 (x1 <> x2) (y1 <> y2)


-- === Functions === --

lengthSquared :: Num a => Vector2 a -> a
lengthSquared (Vector2 x' y') = x' * x' + y' * y'

magnitude :: Floating a => Vector2 a -> a
magnitude = sqrt . lengthSquared

normalize :: Vector2 Double -> Vector2 Double
normalize (Vector2 x' y') = Vector2 (x' / len) (y' / len) where
    len = sqrt $ x' * x' + y' * y'

explode :: Vector2 Double -> Vector2 Double
explode (Vector2 x' y') = Vector2 (fact * x') (fact * y') where
    fact  = shift (\x'' -> 1.0 / (x'' ** 4)) lenSq
    lenSq = x' * x' + y' * y'

shift :: (Double -> Double) -> Double -> Double
shift f x' = if x' < shiftConst then 0.0
                                else f (x' - shiftConst)
    where shiftConst = 0.1

dotV :: Num a => Vector2 a -> Vector2 a-> a
dotV (Vector2 x1 x2) (Vector2 y1 y2) = x1 * y1 + x2 * y2

negateSnd :: Num a => Vector2 a -> Vector2 a
negateSnd (Vector2 x' y') = Vector2 x' (-y')

fromTuple :: (a, a) -> Vector2 a
fromTuple (a, b) = Vector2 a b

toTuple :: Vector2 a -> (a, a)
toTuple (Vector2 a b) = (a, b)

minMax :: Ord a => Vector2 a -> Vector2 a -> Vector2 a
minMax (Vector2 a b) (Vector2 a' b') = Vector2 (min a a') (max b b')

maxMin :: Ord a => Vector2 a -> Vector2 a -> Vector2 a
maxMin (Vector2 a b) (Vector2 a' b') = Vector2 (max a a') (min b b')

scalarProduct :: Num a => Vector2 a -> a -> Vector2 a
scalarProduct vec k = vec & x *~ k & y *~ k
