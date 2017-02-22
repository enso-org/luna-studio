module Luna.Studio.React.Model.Searcher where

import           Data.ScreenPosition            (ScreenPosition)
import           Empire.API.Data.Node           (Node, NodeId)
import           Luna.Studio.Prelude
import           Text.ScopeSearcher.QueryResult (QueryResult)
import qualified Text.ScopeSearcher.QueryResult as Result


data Mode = Command [QueryResult ()]
          | Node [QueryResult Node]
          deriving (Eq, Generic, Show)

data Searcher = Searcher
      { _position    :: ScreenPosition
      , _selected    :: Int
      , _mode        :: Mode
      , _input       :: Text
      , _nodeId      :: Maybe NodeId
      } deriving (Eq, Generic, Show)

makeLenses ''Searcher

mkDef :: Mode -> Searcher
mkDef mode' = Searcher def def mode' def def

defNode, defCommand :: Searcher
defNode    = mkDef $ Node def
defCommand = mkDef $ Command def

selectedExpression :: Getter Searcher Text
selectedExpression = to getExpression where
    getExpression searcher = expression where
        selected'  = searcher ^. selected
        mayResult = listToMaybe $ drop selected' $ case searcher ^. mode of
            Command results -> Result._name <$> results
            Node    results -> Result._name <$> results
        expression = fromMaybe (searcher ^. input) mayResult

selectedNode :: Getter Searcher (Maybe Node)
selectedNode = to getNode where
    getNode searcher = mayNode where
        selected' = searcher ^. selected
        mayNode   = listToMaybe $ drop selected' $ case searcher ^. mode of
            Node results -> Result._element <$> results
            _            -> def

resultsLength :: Getter Searcher Int
resultsLength = to getLength where
    getLength searcher = case searcher ^. mode of
      Command results -> length results
      Node    results -> length results

isNode :: Getter Searcher Bool
isNode = to matchNode where
    matchNode searcher = case searcher ^. mode of
        Node {} -> True
        _       -> False
