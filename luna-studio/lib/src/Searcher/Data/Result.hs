module Searcher.Data.Result where

import Common.Prelude

import Searcher.Data.Class (SearcherData (text),
                            SearcherHint (documentation, prefix))
import Searcher.Data.Match (Match)


--------------------
-- === Result === --
--------------------

-- === Definition === --

data Result a = Result
    { _hint :: a
    , _score :: Double
    , _match :: Match
    } deriving (Functor, Show, Eq, Generic)
makeLenses ''Result

instance SearcherData a => SearcherData (Result a) where
    text = hint . text

instance SearcherHint a => SearcherHint (Result a) where
    prefix        = hint . prefix
    documentation = hint . documentation

instance NFData a => NFData (Result a)

-- === Construction === --

make :: a -> Result a
make = \a -> Result a 0 def
{-# INLINE make #-}
