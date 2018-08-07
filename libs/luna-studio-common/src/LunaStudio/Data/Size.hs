module LunaStudio.Data.Size
    ( module LunaStudio.Data.Size
    , vector
    ) where

import           Control.Lens            (makeWrapped)
import           Data.Aeson.Types        (ToJSON)
import           LunaStudio.Data.Vector2
import           Prologue


-------------------
-- === Size === ---
-------------------

-- === Definition === --

newtype Size = Size (Vector2 Double) deriving (Eq, Generic, Show)
makeWrapped ''Size


height :: Lens' Size Double
height = y

width :: Lens' Size Double
width = x

-- === Instances === --

type instance VectorOf Size = Vector2 Double

instance IsVector Size
instance Dim1     Size
instance Dim2     Size
instance Default  Size
instance ToJSON   Size

type instance Item Size = Double
instance Convertible Size [Double] where convert = toList . view vector
instance Convertible [Double] Size where convert = Size . fromList


-- === Functions === ---

fromTuple :: (Double, Double) -> Size
fromTuple = uncurry fromDoubles

fromDoubles :: Double -> Double -> Size
fromDoubles = Size .: Vector2

toTuple :: Size -> (Double, Double)
toTuple (Size (Vector2 x' y')) = (x', y')
