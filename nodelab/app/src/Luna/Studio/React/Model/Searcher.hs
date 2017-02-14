module Luna.Studio.React.Model.Searcher where

import           Data.Position                  (Position)
import           Empire.API.Data.Node           (NodeId)
import           Luna.Studio.Prelude
import           Text.ScopeSearcher.QueryResult (QueryResult)
import qualified Text.ScopeSearcher.QueryResult as Result


data Mode = Command
          | Node
          deriving (Eq, Generic, Show)

data Searcher = Searcher
      { _position    :: Position
      , _selected    :: Int
      , _mode        :: Mode
      , _input       :: Text
      , _results     :: [QueryResult]
      , _nodeId      :: Maybe NodeId
      } deriving (Eq, Generic, Show)

makeLenses ''Searcher

mkDef :: Mode -> Searcher
mkDef mode' = Searcher def def mode' def def def

defNode, defCommand :: Searcher
defNode    = mkDef Node
defCommand = mkDef Command

selectedExpression :: Contravariant f => (Text -> f Text) -> Searcher -> f Searcher
selectedExpression = to getExpression where
    getExpression searcher = expression where
        selected'  = searcher ^. selected
        mayResult = listToMaybe $ drop selected' $ searcher ^. results
        expression = case mayResult of
            Just result -> result ^. Result.name
            Nothing -> searcher ^. input
