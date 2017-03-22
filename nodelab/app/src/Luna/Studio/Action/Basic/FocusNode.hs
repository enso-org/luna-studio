module Luna.Studio.Action.Basic.FocusNode where

import           Data.Ord                                    (comparing)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (getExpressionNodes, modifyExpressionNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpressionNode, NodeId, nodeId, zPos)
import           Luna.Studio.State.Global                    (State, ui)
import           Luna.Studio.State.UI                        (topZIndex)


focusNode :: NodeId -> Command State ()
focusNode = focusNodes . return

focusNodes :: [NodeId] -> Command State ()
focusNodes nodeIds = use (ui . topZIndex) >>= \topZ -> do
    let zIndexes = zip nodeIds $ map fromIntegral [topZ..]
    forM_ zIndexes $ \(nid, idx) -> modifyExpressionNode nid $ zPos .= idx
    ui . topZIndex .= topZ + (length nodeIds)

updateNodeZOrder :: Command State ()
updateNodeZOrder = do
    let sortNodes :: [ExpressionNode] -> [NodeId]
        sortNodes = map (view nodeId) . sortBy (comparing $ view zPos)
    ui . topZIndex .= def
    getExpressionNodes >>= (focusNodes . sortNodes)
