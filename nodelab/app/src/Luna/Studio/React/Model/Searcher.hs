module Luna.Studio.React.Model.Searcher where

import           Data.Position                  (Position)
import           Empire.API.Data.Node           (NodeId)
import           Luna.Studio.Prelude

import           Text.ScopeSearcher.QueryResult (QueryResult)




data Searcher = Searcher
      { _visible     :: Bool
      , _position    :: Position
      , _selected    :: Int
      , _input       :: Text
      , _results     :: [QueryResult]
      , _nodeId      :: Maybe NodeId
      } deriving (Show, Generic, Eq)

makeLenses ''Searcher

instance Default Searcher where
    def = Searcher False def def def def def
