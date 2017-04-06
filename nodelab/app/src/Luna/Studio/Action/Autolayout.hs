{-# LANGUAGE MultiWayIf #-}
module Luna.Studio.Action.Autolayout where

import           Data.Map.Lazy                               (Map)
import qualified Data.Map.Lazy                               as Map
import           Data.Position                               (Position, move)
import           Data.Vector                                 (Vector2 (Vector2))
import           Luna.Studio.Action.Basic                    (moveNodes)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (getConnectionsFromNode, getConnectionsToNode, getExpressionNode,
                                                              getExpressionNodes, getSelectedNodes)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection          (Connection, dstNodeLoc, dstPortId)
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpressionNode, NodeLoc, nodeLoc, position)
import           Luna.Studio.React.Model.Port                (PortId (InPortId), isSelf)
import           Luna.Studio.State.Global                    (State)


data NodesChain = NodesChain { _nodes    :: [ExpressionNode]
                             , _outConns :: [Connection]
                             , _inConns  :: [Connection]
                             , _len      :: Int
                             } deriving (Eq, Generic, Show)
makeLenses ''NodesChain

gapBetweenNodes :: Double
gapBetweenNodes = 150

autolayoutAllNodes :: Command State ()
autolayoutAllNodes = map (view nodeLoc) <$> getExpressionNodes >>= autolayoutNodes

autolayoutSelectedNodes :: Command State ()
autolayoutSelectedNodes = map (view nodeLoc) <$> getSelectedNodes >>= autolayoutNodes

autolayoutNodes :: [NodeLoc] -> Command State ()
autolayoutNodes nls = do
    chains <- fmap (mergeChains . Map.fromList . catMaybes) $ forM nls $ \nl -> do
        mayNode <- getExpressionNode nl
        outC    <- getConnectionsFromNode nl
        inC     <- getConnectionsToNode nl
        return $ maybe Nothing (\node -> Just (nl, NodesChain [node] outC inC 1)) mayNode
    alignChains chains

mergeChains :: Map NodeLoc NodesChain -> Map NodeLoc NodesChain
mergeChains chains = foldl proccessByKey chains (Map.keys chains) where
    proccessByKey :: Map NodeLoc NodesChain -> NodeLoc -> Map NodeLoc NodesChain
    proccessByKey result nl = maybe result (proccessChain result) $ Map.lookup nl result
    performMerge :: Map NodeLoc NodesChain -> NodeLoc -> NodesChain -> NodeLoc -> NodesChain -> Map NodeLoc NodesChain
    performMerge chainsMap prefId prefix suffId suffix = flip proccessChain mergedChain $ Map.delete suffId . Map.insert prefId mergedChain $ chainsMap where
        mergedChain = NodesChain (prefix ^. nodes ++ suffix ^. nodes) (suffix ^. outConns) (prefix ^. inConns) (prefix ^. len + suffix ^. len)
    proccessChain :: Map NodeLoc NodesChain -> NodesChain -> Map NodeLoc NodesChain
    proccessChain result chain =
        if (length $ chain ^. outConns) == 1 then do
            let dstNl = view dstNodeLoc . head $ chain ^. outConns
                nl    = view nodeLoc    . head $ chain ^. nodes
                performMerge' dstChain = if length (dstChain ^. inConns) == 1
                    then performMerge result nl chain dstNl dstChain
                    else result
            maybe result performMerge' $ Map.lookup dstNl result
        else if (length $ filter (isSelf . InPortId . view dstPortId) $ chain ^. outConns) == 1 then do
            let dstNl = view dstNodeLoc . head $ chain ^. outConns
                nl    = view nodeLoc    . head $ chain ^. nodes
            maybe result (performMerge result nl chain dstNl) $ Map.lookup dstNl result
        else result

alignChains :: Map NodeLoc NodesChain -> Command State ()
alignChains = moveNodes . concat . map getAlignedChainPositions . Map.elems

getAlignedChainPositions :: NodesChain -> [(NodeLoc, Position)]
getAlignedChainPositions chain = if null $ chain ^. nodes then [] else
    foldl setPos [] $ chain ^. nodes where
        setPos result n = if null result then [(n ^. nodeLoc, n ^. position)] else
            (n ^. nodeLoc, move (Vector2 gapBetweenNodes 0) . snd $ head result) : result
