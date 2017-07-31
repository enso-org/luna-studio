module NodeEditor.Action.Basic.FocusNode where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           Data.Ord                                   (comparing)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNodes, getNodeEditor, modifyExpressionNode, modifyNodeEditor)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, NodeLoc, nodeLoc, zPos)
import           NodeEditor.React.Model.NodeEditor          (topZIndex)
import           NodeEditor.State.Global                    (State)


focusNode :: NodeLoc -> Command State ()
focusNode = focusNodes . return

focusNodes :: [NodeLoc] -> Command State ()
focusNodes nodeLocs = (view topZIndex <$> getNodeEditor) >>= \topZ -> do
    let zIndexes = zip nodeLocs $ map fromIntegral [topZ..]
    forM_ zIndexes $ \(nl, idx) -> modifyExpressionNode nl $ zPos .= idx
    modifyNodeEditor $ topZIndex .= topZ + (length nodeLocs)

updateNodeZOrder :: Command State ()
updateNodeZOrder = do
    let sortNodes :: [ExpressionNode] -> [NodeLoc]
        sortNodes = map (view nodeLoc) . sortBy (comparing $ view zPos)
    modifyNodeEditor $ topZIndex .= def
    getExpressionNodes >>= (focusNodes . sortNodes)
