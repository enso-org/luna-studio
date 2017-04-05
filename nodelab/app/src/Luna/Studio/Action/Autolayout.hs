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
        return $ maybe Nothing (\node -> Just (nl, NodesChain [node] outC inC)) mayNode
    alignChains chains

mergeChains :: Map NodeLoc NodesChain -> Map NodeLoc NodesChain
mergeChains chains = foldl proccessByKey chains (Map.keys chains) where
    proccessByKey :: Map NodeLoc NodesChain -> NodeLoc -> Map NodeLoc NodesChain
    proccessByKey result nl = maybe result (proccessChain result) $ Map.lookup nl result
    proccessChain :: Map NodeLoc NodesChain -> NodesChain -> Map NodeLoc NodesChain
    proccessChain result chain = do
        if (length $ chain ^. outConns) == 1 then do
            let dstNl = view dstNodeLoc . head $ chain ^. outConns
                nl    = view nodeLoc    . head $ chain ^. nodes
            case Map.lookup dstNl result of
                Nothing       -> result
                Just dstChain -> do
                    let dstInConns = dstChain ^. inConns
                    if length dstInConns == 1 then do
                        let mergedChain = NodesChain (chain ^. nodes ++ dstChain ^. nodes) (dstChain ^. outConns) (chain ^. inConns)
                        flip proccessChain mergedChain $ Map.delete dstNl . Map.insert nl mergedChain $ result
                    else result
        else if (length $ filter (isSelf . InPortId . view dstPortId) $ chain ^. outConns) == 1 then do
            let dstNl = view dstNodeLoc . head $ chain ^. outConns
                nl    = view nodeLoc    . head $ chain ^. nodes
            case Map.lookup dstNl result of
                Nothing       -> result
                Just dstChain -> flip proccessChain mergedChain $ Map.delete dstNl . Map.insert nl mergedChain $ result where
                    mergedChain = NodesChain (chain ^. nodes ++ dstChain ^. nodes) (dstChain ^. outConns) (chain ^. inConns)
        else result

alignChains :: Map NodeLoc NodesChain -> Command State ()
alignChains = moveNodes . concat . map getAlignedChainPositions . Map.elems

getAlignedChainPositions :: NodesChain -> [(NodeLoc, Position)]
getAlignedChainPositions chain = if null $ chain ^. nodes then [] else
    foldl setPos [] $ chain ^. nodes where
        setPos result n = if null result then [(n ^. nodeLoc, n ^. position)] else
            (n ^. nodeLoc, move (Vector2 gapBetweenNodes 0) . snd $ head result) : result
