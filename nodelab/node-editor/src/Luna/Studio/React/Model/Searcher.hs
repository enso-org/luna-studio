module Luna.Studio.React.Model.Searcher where

import           Data.Position                  (Position)
import           Empire.API.Data.Node           (ExpressionNode)
import           Empire.API.Data.NodeLoc        (NodeLoc)
import           Luna.Prelude
import           Text.ScopeSearcher.QueryResult (QueryResult)
import qualified Text.ScopeSearcher.QueryResult as Result


data Mode = Command [QueryResult ()]
          | Node [QueryResult ExpressionNode]
          deriving (Eq, Generic, Show)

data Searcher = Searcher
      { _position      :: Position
      , _selected      :: Int
      , _mode          :: Mode
      , _input         :: Text
      , _nodeLoc       :: Maybe NodeLoc
      , _rollbackReady :: Bool
      } deriving (Eq, Generic, Show)

makeLenses ''Searcher

mkDef :: Mode -> Searcher
mkDef mode' = Searcher def def mode' def def False

defNode, defCommand :: Searcher
defNode    = mkDef $ Node def
defCommand = mkDef $ Command def

selectedExpression :: Getter Searcher Text
selectedExpression = to getExpression where
    getExpression searcher = expression where
        selected' = searcher ^. selected
        mayResult = if selected' == 0 then Just $ searcher ^. input else
            listToMaybe $ drop (selected' - 1) $ case searcher ^. mode of
                Command results -> Result._name <$> results
                Node    results -> Result._name <$> results
        expression = fromMaybe (searcher ^. input) mayResult

selectedNode :: Getter Searcher (Maybe ExpressionNode)
selectedNode = to getNode where
    getNode searcher = mayNode where
        selected' = searcher ^. selected
        mayNode   = if selected' == 0 then Nothing else
            listToMaybe $ drop (selected' - 1) $ case searcher ^. mode of
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
