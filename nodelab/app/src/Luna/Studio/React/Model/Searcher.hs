module Luna.Studio.React.Model.Searcher where

import           Data.Position                  (Position)
import           Empire.API.Data.Node           (NodeId)
import           Luna.Studio.Prelude            hiding (Context)

import           Text.ScopeSearcher.QueryResult (QueryResult)
import qualified Text.ScopeSearcher.QueryResult as Result


data Context = Command
             | Node [Text]
             deriving (Eq, Generic, Show)

data Searcher = Searcher
      { _position    :: Position
      , _selected    :: Int
      , _context     :: Context
      , _input       :: Text
      , _results     :: [QueryResult]
      , _nodeId      :: Maybe NodeId
      } deriving (Eq, Generic, Show)

makeLenses ''Searcher

mkDef :: Context -> Searcher
mkDef ctx = Searcher def def ctx def def def

defNode, defCommand :: Searcher
defNode    = mkDef $ Node def
defCommand = mkDef Command

selectedExpression :: Getter Searcher Text
selectedExpression = to getExpression where
    getExpression searcher = expression where
        selected'  = searcher ^. selected
        mayResult = listToMaybe $ drop selected' $ searcher ^. results
        expression = case mayResult of
            Just result -> result ^. Result.name
            Nothing -> searcher ^. input
