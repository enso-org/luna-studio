module Luna.Studio.React.Model.Searcher where

import           Empire.API.Data.Node           (NodeId)
import           Text.ScopeSearcher.QueryResult (QueryResult)
import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector



data Searcher = Searcher
      { _visible     :: Bool
      , _position    :: Vector2 Int
      , _selected    :: Int
      , _input       :: Text
      , _results     :: [QueryResult]
      , _nodeId      :: Maybe NodeId
      } deriving (Show, Generic)

makeLenses ''Searcher

instance Default Searcher where
    def = Searcher False def def def def def
