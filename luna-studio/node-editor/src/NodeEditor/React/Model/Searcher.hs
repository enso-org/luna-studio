module NodeEditor.React.Model.Searcher where

import           Common.Prelude
import           LunaStudio.Data.Node           (ExpressionNode)
import           LunaStudio.Data.NodeLoc        (NodeLoc)
import qualified LunaStudio.Data.NodeLoc        as NodeLoc
import           LunaStudio.Data.PortRef        (OutPortRef, srcNodeLoc)
import           LunaStudio.Data.Position       (Position)
import           Text.ScopeSearcher.QueryResult (QueryResult)
import qualified Text.ScopeSearcher.QueryResult as Result


data Mode = Command                            [QueryResult ()]
          | Node     NodeLoc  (Maybe Position) [QueryResult ExpressionNode]
          | NodeName NodeLoc                   [QueryResult ExpressionNode]
          | PortName OutPortRef                [QueryResult ExpressionNode]
          deriving (Eq, Generic, Show)

data Searcher = Searcher
      { _selected      :: Int
      , _mode          :: Mode
      , _input         :: Text
      , _replaceInput  :: Bool
      , _rollbackReady :: Bool
      } deriving (Eq, Generic, Show)

makeLenses ''Searcher

mkDef :: Mode -> Searcher
mkDef mode' = Searcher def mode' def False False

defCommand :: Searcher
defCommand = mkDef $ Command def

selectedExpression :: Getter Searcher Text
selectedExpression = to getExpression where
    getExpression searcher = expression where
        selected' = searcher ^. selected
        mayResult = if selected' == 0 then Just $ searcher ^. input else
            listToMaybe $ drop (selected' - 1) $ case searcher ^. mode of
                Command      results -> Result._name <$> results
                Node     _ _ results -> Result._name <$> results
                NodeName _   results -> Result._name <$> results
                PortName _   results -> Result._name <$> results
        expression = fromMaybe (searcher ^. input) mayResult

selectedNode :: Getter Searcher (Maybe ExpressionNode)
selectedNode = to getNode where
    getNode searcher = mayNode where
        selected' = searcher ^. selected
        mayNode   = if selected' == 0 then Nothing else
            listToMaybe $ drop (selected' - 1) $ case searcher ^. mode of
                Node _ _ results -> Result._element <$> results
                _                -> def

resultsLength :: Getter Searcher Int
resultsLength = to getLength where
    getLength searcher = case searcher ^. mode of
      Command      results -> length results
      Node     _ _ results -> length results
      NodeName _   results -> length results
      PortName _   results -> length results

updateNodeResult :: [QueryResult ExpressionNode] -> Mode -> Mode
updateNodeResult r (Node nl pos _) = Node nl pos r
updateNodeResult _ m               = m

isCommand :: Getter Searcher Bool
isCommand = to matchCommand where
  matchCommand searcher = case searcher ^. mode of
      Command _ -> True
      _         -> False

isNode :: Getter Searcher Bool
isNode = to matchNode where
    matchNode searcher = case searcher ^. mode of
        Node _ _ _ -> True
        _          -> False

isNodeName :: Getter Searcher Bool
isNodeName = to matchNodeName where
    matchNodeName searcher = case searcher ^. mode of
        NodeName _ _ -> True
        _            -> False

isSearcherRelated :: NodeLoc -> Searcher -> Bool
isSearcherRelated nl s = isPrefixOf nlIdPath sIdPath where
    nlIdPath = NodeLoc.toNodeIdList nl
    sIdPath = case s ^. mode of
        Node     snl   _ _ -> NodeLoc.toNodeIdList snl
        NodeName snl     _ -> NodeLoc.toNodeIdList snl
        PortName portRef _ -> NodeLoc.toNodeIdList $ portRef ^. srcNodeLoc
        _                  -> []
