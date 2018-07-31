module NodeEditor.Action.Basic.CreateGraph where

import           Common.Action.Command                 (Command)
import           Common.Prelude
import qualified Data.Set                              as Set
import           LunaStudio.Data.Connection            (Connection)
import qualified LunaStudio.Data.Connection            as Connection
import           LunaStudio.Data.Graph                 (Graph)
import qualified LunaStudio.Data.Graph                 as API
import           LunaStudio.Data.MonadPath             (MonadPath)
import           LunaStudio.Data.NodeLoc               (NodePath)
import           NodeEditor.Action.Basic.AddConnection (localAddConnections)
import           NodeEditor.Action.Basic.FocusNode     (updateNodeZOrder)
import           NodeEditor.Action.Basic.RemoveNode    (localRemoveNodes)
import           NodeEditor.Action.Basic.UpdateNode    (localUpdateOrAddExpressionNode, localUpdateOrAddInputNode,
                                                        localUpdateOrAddOutputNode)
import           NodeEditor.Action.State.NodeEditor    (getExpressionNodes, modifyNodeEditor,
                                                        setGraphStatus, updateMonads)
import           NodeEditor.React.Model.Node           (ExpressionNode, InputNode, OutputNode, nodeLoc)
import qualified NodeEditor.React.Model.NodeEditor     as NE
import           NodeEditor.State.Global               (State)



updateWithAPIGraph :: NodePath -> Graph -> Command State ()
updateWithAPIGraph p g = updateGraph nodes input output conns monads
    >> setGraphStatus NE.GraphLoaded where
        nodes  = convert . (p,) <$> g ^. API.nodes
        input  = convert . (p,) <$> g ^. API.inputSidebar
        output = convert . (p,) <$> g ^. API.outputSidebar
        conns  = Connection.prependPath p <$> g ^. API.connections
        monads = g ^. API.monads


updateGraph :: [ExpressionNode] -> Maybe InputNode -> Maybe OutputNode
    -> [Connection] -> [MonadPath] -> Command State ()
updateGraph nodes input output connections monads = do
    let nlsSet = Set.fromList $ map (view nodeLoc) nodes
    nlsToRemove <- filter (not . flip Set.member nlsSet) . map (view nodeLoc)
        <$> getExpressionNodes
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
