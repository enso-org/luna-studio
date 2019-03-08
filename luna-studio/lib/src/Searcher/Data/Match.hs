module Searcher.Data.Match where

import Common.Prelude hiding (length)

data Range = Range
    { _start  :: Int
    , _length :: Int
    } deriving (Show, Eq, Generic)
makeLenses ''Range
instance NFData Range

letterRange :: Int -> Range
letterRange = flip Range 1

newtype Match = Match [Range] deriving (Show, Eq, Generic)
makeWrapped ''Match
instance NFData Match

instance Default Match where def = Match []

fromList :: [Int] -> Match
fromList matchedPositions = Match $ go Nothing matchedPositions where
    go (Just r) []           = [r]
    go Nothing  []           = []
    go (Just r) (pos : poss) =
        if pos == view start r + view length r
            then go (Just $ r & length +~ 1) poss
            else r : go (Just $ letterRange pos) poss
    go Nothing  (pos : poss) = go (Just $ letterRange pos) poss
