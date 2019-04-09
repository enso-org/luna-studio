module Searcher.Data.Match where

import Common.Prelude hiding (length)

-------------------
-- === Range === --
-------------------

-- === Definition === --

data Range = Range
    { _start  :: Int
    , _length :: Int
    } deriving (Show, Eq, Generic)
makeLenses ''Range
instance NFData Range

-- === Construction === --

letterRange :: Int -> Range
letterRange = flip Range 1

-------------------
-- === Range === --
-------------------

-- === Definition === --

data MatchSource
    = None
    | Expression
    | Tag
    deriving (Show, Eq, Generic)
makePrisms ''MatchSource
instance NFData MatchSource

data Match = Match
    { _source :: MatchSource
    , _range  :: [Range]
    } deriving (Show, Eq, Generic)
makeLenses ''Match
instance NFData Match

instance Default Match where
    def = Match None mempty

-- === Construction === --

make :: MatchSource -> [Int] -> Match
make source matchedPositions = Match source $ go Nothing matchedPositions where
    go (Just r) []           = [r]
    go Nothing  []           = []
    go (Just r) (pos : poss) =
        if pos == view start r + view length r
            then go (Just $ r & length +~ 1) poss
            else r : go (Just $ letterRange pos) poss
    go Nothing  (pos : poss) = go (Just $ letterRange pos) poss
