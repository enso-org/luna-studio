module Node.Editor.Action.Basic.FocusNode where

import           Data.Ord                                    (comparing)
import           Node.Editor.Action.Command                  (Command)
import           Node.Editor.Action.State.NodeEditor         (getExpressionNodes, modifyExpressionNode)
import           Luna.Prelude
import           Node.Editor.React.Model.Node.ExpressionNode (ExpressionNode, NodeLoc, nodeLoc, zPos)
import           Node.Editor.State.Global                    (State, ui)
import           Node.Editor.State.UI                        (topZIndex)


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
