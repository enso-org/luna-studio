{-# LANGUAGE TypeFamilies #-}
module Data.ScreenPosition
    ( module Data.ScreenPosition
    , vector
    , x
    , y
    )
where

import           Common.Prelude
import           Data.Aeson              (FromJSON, ToJSON)
import           LunaStudio.Data.Vector2
-----------------------------
-- === ScreenPosition === ---
-----------------------------

-- === Definition === --

newtype ScreenPosition = ScreenPosition { fromScreenPosition :: Vector2 Double } deriving (Eq, Show, Generic, Default, NFData, Num)
makeWrapped ''ScreenPosition


-- === Instances === --

type instance VectorOf ScreenPosition = Vector2 Double

instance Dim1      ScreenPosition
instance Dim2      ScreenPosition
instance FromJSON  ScreenPosition
instance IsVector  ScreenPosition
instance ToJSON    ScreenPosition

type instance Item ScreenPosition = Double
instance ToList    ScreenPosition where toList   = toList . view vector
instance FromList  ScreenPosition where fromList = ScreenPosition . fromList


-- === Functions === ---

fromTuple :: (Double, Double) -> ScreenPosition
fromTuple = uncurry fromDoubles

fromDoubles :: Double -> Double -> ScreenPosition
fromDoubles = ScreenPosition .: Vector2

toTuple :: ScreenPosition -> (Double, Double)
toTuple (ScreenPosition (Vector2 x' y')) = (x', y')

move :: Vector2 Double -> ScreenPosition -> ScreenPosition
move vec pos = pos & vector +~ vec
