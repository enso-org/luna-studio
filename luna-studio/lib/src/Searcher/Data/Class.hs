module Searcher.Data.Class where

import Common.Prelude

class SearcherData a where
    text :: Getter a Text
    tags :: Getter a [Text]

class SearcherData a => SearcherHint a where
    prefix        :: Getter a Text
    documentation :: Getter a Text
