module Luna.Studio.Action.Graph.Focus
    ( focusNode
    , focusNodes
    , updateNodeZOrder
    ) where

import           Data.Ord                           (comparing)
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Graph.Lookup    (allNodes)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node       (Node, NodeId)
import qualified Luna.Studio.React.Model.Node       as Node
import qualified Luna.Studio.React.Model.Node       as Model
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global



focusNode :: NodeId -> Command State ()
focusNode = focusNodes . return

focusNodes :: [NodeId] -> Command State ()
focusNodes nodeIds = do
    topZIndex <- use Global.topZIndex
    let newTopZIndex = topZIndex + length nodeIds
        zIndexes = [topZIndex..newTopZIndex]
    Global.modifyNodeEditor $
        forM_ (zip nodeIds zIndexes) $ \(nodeId, idx) -> do
            let newZPos = fromIntegral idx
            NodeEditor.nodes . at nodeId %= fmap (Node.zPos .~ newZPos)
    Global.topZIndex .= newTopZIndex

sortNodes :: [Node] -> [Node]
sortNodes = sortBy (comparing $ view Model.zPos)

sortNodes' :: [Node] -> [NodeId]
sortNodes' = map (view Model.nodeId) . sortNodes

updateNodeZOrder :: Command State ()
updateNodeZOrder = do
    Global.topZIndex .= def
    sortedNodeIds <- sortNodes' <$> allNodes
    focusNodes sortedNodeIds
