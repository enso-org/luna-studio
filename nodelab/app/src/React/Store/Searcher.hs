module React.Store.Searcher (
    module React.Store.Searcher,
) where

import           Text.ScopeSearcher.QueryResult (QueryResult)
import           Utils.PreludePlus
import           Utils.Vector



data Searcher = Searcher
      { _visible     :: Bool
      , _position    :: Vector2 Int
      , _selected    :: Int
      , _input       :: Text
      , _results     :: [QueryResult]
      } deriving (Show, Generic)

makeLenses ''Searcher

instance Default Searcher where
    def = Searcher False def def def def
