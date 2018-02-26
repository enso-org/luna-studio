module NodeEditor.Action.Basic.CreateGraph where

import           Common.Action.Command                 (Command)
import           Common.Prelude
import qualified Data.Set                              as Set
import           LunaStudio.Data.MonadPath             (MonadPath)
import           LunaStudio.Data.PortRef               (InPortRef, OutPortRef)
import           NodeEditor.Action.Basic.AddConnection (localAddConnections)
import           NodeEditor.Action.Basic.AddNode       (localAddExpressionNodes)
import           NodeEditor.Action.Basic.FocusNode     (updateNodeZOrder)
import           NodeEditor.Action.Basic.RemoveNode    (localRemoveNodes)
import           NodeEditor.Action.Basic.UpdateNode    (localUpdateOrAddExpressionNode, localUpdateOrAddInputNode,
                                                        localUpdateOrAddOutputNode)
import           NodeEditor.Action.State.NodeEditor    (addInputNode, addOutputNode, getExpressionNodes, modifyNodeEditor, resetGraph,
                                                        updateMonads)
import           NodeEditor.React.Model.Node           (ExpressionNode, InputNode, OutputNode, nodeLoc)
import qualified NodeEditor.React.Model.NodeEditor     as NE
import           NodeEditor.State.Global               (State)


createGraph :: [ExpressionNode] -> Maybe InputNode -> Maybe OutputNode -> [(OutPortRef, InPortRef)] -> [MonadPath] -> Command State ()
createGraph nodes input output connections monads = do
    resetGraph
    localAddExpressionNodes nodes
    mapM_ addInputNode input
    mapM_ addOutputNode output
    void $ localAddConnections connections
    updateMonads monads
    updateNodeZOrder

updateGraph :: [ExpressionNode] -> Maybe InputNode -> Maybe OutputNode -> [(OutPortRef, InPortRef)] -> [MonadPath] -> Command State ()
updateGraph nodes input output connections monads = do
    let nlsSet = Set.fromList $ map (view nodeLoc) nodes
    nlsToRemove <- filter (not . flip Set.member nlsSet) . map (view nodeLoc) <$> getExpressionNodes
    void $ localRemoveNodes nlsToRemove
    mapM_ (localUpdateOrAddExpressionNode def) nodes

    case input of
        Nothing -> modifyNodeEditor $ NE.inputNode .= def
        Just n  -> localUpdateOrAddInputNode n

    case output of
        Nothing -> modifyNodeEditor $ NE.outputNode .= def
        Just n  -> localUpdateOrAddOutputNode n

    modifyNodeEditor $ NE.connections .= def
    void $ localAddConnections connections

    updateMonads monads
    updateNodeZOrder
