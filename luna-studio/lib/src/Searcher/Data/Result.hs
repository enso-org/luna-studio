module Searcher.Data.Result where

import Common.Prelude

import Data.Ord            (comparing)
import Searcher.Data.Class (SearcherData (text, tags),
                            SearcherHint (documentation, prefix))
import Searcher.Data.Match (Match)


--------------------
-- === Result === --
--------------------

-- === Definition === --

data Result a = Result
    { _id    :: Int
    , _hint  :: a
    , _score :: Double
    , _match :: Match
    } deriving (Functor, Show, Eq, Generic)
makeLenses ''Result

instance SearcherData a => SearcherData (Result a) where
    text = hint . text
    tags = hint . tags

instance SearcherHint a => SearcherHint (Result a) where
    prefix        = hint . prefix
    documentation = hint . documentation

instance NFData a => NFData (Result a)

-- === Construction === --

make :: Int -> a -> Result a
make = \id a -> Result id a 0 def
{-# INLINE make #-}

-- === Utils === --

maxByScore :: Result a -> Result a -> Result a
maxByScore res1 res2 = if res1 ^. score > res2 ^. score then res1 else res2
