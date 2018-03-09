module NodeEditor.View.NodeEditor where

import           Common.Prelude
import           NodeEditor.React.Model.NodeEditor (NodeEditor, connections, expressionNodes, inputNode, searcher, outputNode)
import           NodeEditor.View.Connection        (connectionsView)
import           NodeEditor.View.ExpressionNode    (expressionNodesView)
import           NodeEditor.View.Searcher          (searcherView)
import           NodeEditor.View.SidebarNode       (inputNodeView, outputNodeView)
import           NodeEditor.View.Diff              (DiffT, diff)


nodeEditorView :: MonadIO m => DiffT NodeEditor m ()
nodeEditorView = do
    diff expressionNodesView expressionNodes
    diff inputNodeView       inputNode
    diff outputNodeView      outputNode
    diff searcherView        searcher
    diff connectionsView     connections
