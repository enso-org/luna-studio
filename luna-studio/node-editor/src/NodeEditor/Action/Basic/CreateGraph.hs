module NodeEditor.Action.Basic.CreateGraph where

import Common.Prelude

import qualified Data.Set                              as Set
import qualified LunaStudio.Data.Connection            as Connection
import qualified LunaStudio.Data.Graph                 as API
import qualified NodeEditor.React.Model.NodeEditor     as NE
import qualified LunaStudio.Data.Searcher.Hint.Library as Library

import Common.Action.Command                       (Command)
import Data.Set                                    (Set)
import LunaStudio.Data.Connection                  (Connection)
import LunaStudio.Data.Graph                       (Graph)
import LunaStudio.Data.MonadPath                   (MonadPath)
import LunaStudio.Data.NodeLoc                     (NodePath)
import NodeEditor.Action.Basic.AddConnection       (localAddConnections)
import NodeEditor.Action.Basic.FocusNode           (updateNodeZOrder)
import NodeEditor.Action.Basic.RemoveNode          (localRemoveNodes)
import NodeEditor.Action.Basic.UpdateNode          (localUpdateOrAddExpressionNode,
                                                    localUpdateOrAddInputNode,
                                                    localUpdateOrAddOutputNode)
import NodeEditor.Action.Basic.UpdateSearcherHints (setImportedLibraries)
import NodeEditor.Action.State.NodeEditor          (getExpressionNodes,
                                                    modifyNodeEditor,
                                                    setGraphStatus,
                                                    updateMonads)
import NodeEditor.React.Model.Node                 (ExpressionNode, InputNode,
                                                    OutputNode, nodeLoc)
import NodeEditor.State.Global                     (State)



updateWithAPIGraph :: NodePath -> Graph -> Command State ()
updateWithAPIGraph p g = updateGraph nodes input output conns monads imports
    >> setGraphStatus NE.GraphLoaded where
        nodes   = convert . (p,) <$> g ^. API.nodes
        input   = convert . (p,) <$> g ^. API.inputSidebar
        output  = convert . (p,) <$> g ^. API.outputSidebar
        conns   = Connection.prependPath p <$> g ^. API.connections
        monads  = g ^. API.monads
        imports = g ^. API.imports


updateGraph :: [ExpressionNode] -> Maybe InputNode -> Maybe OutputNode
    -> [Connection] -> [MonadPath] -> Set Library.Name -> Command State ()
updateGraph nodes input output connections monads imports = do
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
    setImportedLibraries imports
    updateNodeZOrder
