module Searcher.Data.Class where

import Common.Prelude

class SearcherData a where
    text     :: Getter a Text
    tags     :: Getter a [Text]
    priority :: Getter a (Maybe Int)

class SearcherData a => SearcherHint a where
    prefix        :: Getter a Text
    documentation :: Getter a Text
