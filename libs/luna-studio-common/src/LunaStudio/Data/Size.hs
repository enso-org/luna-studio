{-# LANGUAGE TypeFamilies #-}
module LunaStudio.Data.Size
    ( module LunaStudio.Data.Size
    , vector
    ) where

import           LunaStudio.Data.Vector2
import           Prologue


-------------------
-- === Size === ---
-------------------

-- === Definition === --

newtype Size = Size (Vector2 Double) deriving (Eq, Show, Generic, Default)
makeWrapped ''Size


height :: Lens' Size Double
height = y

width :: Lens' Size Double
width = x

-- === Instances === --

type instance VectorOf Size = Vector2 Double

instance IsVector Size
instance Dim1 Size
instance Dim2 Size

type instance Item Size = Double
instance ToList    Size where toList   = toList . view vector
instance FromList  Size where fromList = Size . fromList


-- === Functions === ---

fromTuple :: (Double, Double) -> Size
fromTuple = uncurry fromDoubles

fromDoubles :: Double -> Double -> Size
fromDoubles = Size .: Vector2

toTuple :: Size -> (Double, Double)
toTuple (Size (Vector2 x' y')) = (x', y')
