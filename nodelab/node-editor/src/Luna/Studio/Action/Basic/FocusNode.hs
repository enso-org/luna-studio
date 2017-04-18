module Luna.Studio.Action.Basic.FocusNode where

import           Data.Ord                                    (comparing)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.NodeEditor         (getExpressionNodes, modifyExpressionNode)
import           Luna.Prelude
import           Luna.Studio.React.Model.Node.ExpressionNode (ExpressionNode, NodeLoc, nodeLoc, zPos)
import           Luna.Studio.State.Global                    (State, ui)
import           Luna.Studio.State.UI                        (topZIndex)


focusNode :: NodeLoc -> Command State ()
focusNode = focusNodes . return

focusNodes :: [NodeLoc] -> Command State ()
focusNodes nodeLocs = use (ui . topZIndex) >>= \topZ -> do
    let zIndexes = zip nodeLocs $ map fromIntegral [topZ..]
    forM_ zIndexes $ \(nl, idx) -> modifyExpressionNode nl $ zPos .= idx
    ui . topZIndex .= topZ + (length nodeLocs)

updateNodeZOrder :: Command State ()
updateNodeZOrder = do
    let sortNodes :: [ExpressionNode] -> [NodeLoc]
        sortNodes = map (view nodeLoc) . sortBy (comparing $ view zPos)
    ui . topZIndex .= def
    getExpressionNodes >>= (focusNodes . sortNodes)
