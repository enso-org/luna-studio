{-# LANGUAGE TypeFamilies #-}
module Data.Size
    ( module Data.Size
    , vector
    , x
    , y
    )
where

import           Data.Aeson          (ToJSON)
import           Data.Vector2
import           Common.Prelude


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


-- === Functions === ---

fromTuple :: (Double, Double) -> Size
fromTuple = uncurry fromDoubles

fromDoubles :: Double -> Double -> Size
fromDoubles = Size .: Vector2

toTuple :: Size -> (Double, Double)
toTuple (Size (Vector2 x' y')) = (x', y')
