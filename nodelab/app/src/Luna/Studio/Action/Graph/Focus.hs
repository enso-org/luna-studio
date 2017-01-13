module Luna.Studio.Action.Graph.Focus
    ( focusNode
    , focusSelectedNode
    ) where

import           Data.Ord                        (comparing)
import           Luna.Studio.Action.Command      (Command)
import           Luna.Studio.Action.Graph.Lookup (allNodes)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node    (Node)
import qualified Luna.Studio.React.Model.Node    as Node
import qualified Luna.Studio.React.Model.Node    as Model
import           Luna.Studio.State.Global        (State)
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import qualified Luna.Studio.State.Global               as Global



nats :: [Integer]
nats = [1..]

focusNode :: Node -> Command State ()
focusNode node = do
    nodes <- allNodes
    let sortedNodes = sortBy (comparing $ negate . (view Model.zPos)) nodes
        newOrderNodes = node : delete node sortedNodes
        newOrderIds  = view Model.nodeId <$> newOrderNodes
    Global.withNodeEditor $
        forM_ (zip newOrderIds nats) $ \(nodeId, idx) -> do
            let newZPos = negate $ (fromIntegral idx) / 100.0
            NodeEditor.nodes . at nodeId %= fmap (Node.zPos .~ newZPos)

focusSelectedNode :: Command State ()
focusSelectedNode = do
    return () --TODO[react]
    -- widgets <- selectedNodes
    -- inRegistry $ UIRegistry.focusedWidget .= (view ref <$> widgets ^? ix 0)
