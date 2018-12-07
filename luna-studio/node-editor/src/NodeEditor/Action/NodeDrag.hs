{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
module NodeEditor.Action.NodeDrag
    ( startNodeDrag
    , nodesDrag
    , handleNodeDragMouseUp
    ) where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           Control.Arrow
import qualified Data.Map                                   as Map
import           LunaStudio.Data.Geometry                   (snap)
import           LunaStudio.Data.NodeLoc                    (NodeLoc)
import           LunaStudio.Data.PortRef                    (InPortRef (InPortRef), OutPortRef (OutPortRef))
import qualified LunaStudio.Data.PortRef                    as PortRef
import           LunaStudio.Data.Position                   (Position, move, vector)
import           NodeEditor.Action.Basic                    (connect, localMoveNodes, moveNodeOnConnection, moveNodes, selectNodes, updatePortsModeForNode, localSetNodesMeta, toMetaUpdate)
import           NodeEditor.Action.State.Action             (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                             updateActionWithKey)
import           NodeEditor.Action.State.Model              (createConnectionModel, getIntersectingConnections)
import           NodeEditor.Action.State.NodeEditor         (getConnection, getExpressionNode, getNode, getNodeEditor, getSelectedNodes,
                                                             modifyConnection, modifyExpressionNode, modifyInPort, modifyNodeEditor,
                                                             modifyOutPort)
import           NodeEditor.React.Model.Connection          (Mode (Dimmed, Highlighted), dst, getConnectionMode, src)
import qualified NodeEditor.React.Model.Connection          as Connection
import           NodeEditor.React.Model.Node.ExpressionNode (inPortAt, inPortAt, inPortsList, isSelected, nodeLoc, outPortAt, position)
import           NodeEditor.React.Model.NodeEditor          (halfConnections, toPosConnection)
import           NodeEditor.React.Model.Port                (isSelf, mode, portId)
import qualified NodeEditor.React.Model.Port                as Port
import           NodeEditor.State.Action                    (Action (begin, continue, end, update), NodeDrag (NodeDrag), nodeDragAction,
                                                             nodeDragNodeLoc, nodeDragNodesStartPos, nodeDragSnappedConnId,
                                                             nodeDragStartPos)
import           NodeEditor.State.Global                    (State)
import           NodeEditor.State.Mouse                     (workspacePosition)
import           React.Flux                                 (MouseEvent)


instance Action (Command State) NodeDrag where
    begin        = beginActionWithKey    nodeDragAction
    continue     = continueActionWithKey nodeDragAction
    update       = updateActionWithKey   nodeDragAction
    end nodeDrag = do
            metaUpdate <- Map.fromList . fmap (view nodeLoc &&& view position)
                <$> getSelectedNodes
            moveNodes metaUpdate
            clearSnappedConnection nodeDrag
            removeActionFromState nodeDragAction


startNodeDrag :: Position -> NodeLoc -> Bool -> Command State ()
startNodeDrag coord nl snapped = do
    mayNode <- getExpressionNode nl
    withJust mayNode $ \node -> do
        unless (node ^. isSelected) $ selectNodes [nl]
        nodes <- getSelectedNodes
        let nodesPos = Map.fromList $ (view nodeLoc &&& view position) <$> nodes
        if snapped then do
            let snappedNodes = Map.map snap nodesPos
            begin $ NodeDrag coord nl snappedNodes Nothing
            void $ localMoveNodes snappedNodes
        else begin $ NodeDrag coord nl nodesPos Nothing

nodesDrag :: MouseEvent -> Bool -> NodeDrag -> Command State ()
nodesDrag evt snapped nodeDrag = do
    coord <- workspacePosition evt
    let mouseStartPos = view nodeDragStartPos      nodeDrag
        draggedNodeLoc = view nodeDragNodeLoc        nodeDrag
        nodesStartPos = view nodeDragNodesStartPos nodeDrag
        delta = coord ^. vector - mouseStartPos ^. vector
        shift' = if snapped then case Map.lookup draggedNodeLoc nodesStartPos of
                     Just pos -> snap (move delta pos) ^. vector - pos ^. vector
                     Nothing  -> delta
                 else delta
    void . localMoveNodes $ Map.map (move shift') nodesStartPos
    snapConnectionsForNodes coord $ Map.keys nodesStartPos

clearSnappedConnection :: NodeDrag -> Command State ()
clearSnappedConnection nodeDrag = do
    let nl = nodeDrag ^. nodeDragNodeLoc
    modifyNodeEditor $ halfConnections .= def
    withJust (nodeDrag ^. nodeDragSnappedConnId) $ \connId -> do
        mayNode <- getNode $ connId ^. PortRef.nodeLoc
        modifyConnection connId $ Connection.mode .= maybe
            Connection.Normal
            (getConnectionMode connId)
            mayNode
        updatePortsModeForNode $ connId ^. PortRef.nodeLoc
        withJustM (getConnection connId)
            $ updatePortsModeForNode . view (src . PortRef.nodeLoc)
        updatePortsModeForNode nl
        continue $ \nodeDrag' ->
            update $ nodeDrag' & nodeDragSnappedConnId .~ Nothing

snapConnectionsForNodes :: Position -> [NodeLoc] -> Command State ()
snapConnectionsForNodes mousePos nodeLocs = when (length nodeLocs == 1)
    $ forM_ nodeLocs $ \nl -> do
        continue clearSnappedConnection
        mayNode <- getExpressionNode nl
        withJust mayNode $ \node -> do
            mayConnId <- getIntersectingConnections node mousePos
            let maySelfPortRef = fmap (InPortRef nl) . find isSelf
                    $ (view portId) <$> inPortsList node
            withJust ((,) <$> mayConnId <*> maySelfPortRef)
                $ \(connId, selfPortRef) -> do
                    let outPortRef  = OutPortRef nl []
                    mayConn       <- getConnection connId
                    mayConnModel1 <- fmap join .
                        mapM (`createConnectionModel` selfPortRef)
                            $ view src <$> mayConn
                    mayConnModel2 <- fmap join
                        $ mapM (createConnectionModel outPortRef)
                            $ view dst <$> mayConn
                    withJust ((,,) <$> mayConn <*> mayConnModel1 <*> mayConnModel2)
                        $ \(conn, connModel1, connModel2) -> do
                            ne <- getNodeEditor
                            let conns = (Connection.mode .~ Highlighted)
                                    <$> [connModel1, connModel2]
                                conns' = mapMaybe (toPosConnection ne) conns
                            modifyNodeEditor
                                $ halfConnections .= map convert conns'
                            let connIdsMatch nodeDrag
                                    = Just connId
                                    == nodeDrag ^. nodeDragSnappedConnId
                            continue $ \nodeDrag ->
                                unless (connIdsMatch nodeDrag)
                                    $ update $ nodeDrag & nodeDragSnappedConnId
                                        ?~ connId
                            modifyConnection connId $ Connection.mode .= Dimmed
                            modifyExpressionNode nl $ do
                                outPortAt [] . mode .= Port.Highlighted
                                inPortAt (selfPortRef ^. PortRef.dstPortId)
                                    . mode .= Port.Highlighted
                            modifyOutPort (conn ^. src)
                                $ mode .= Port.Highlighted
                            modifyInPort (conn ^. dst)
                                $ mode .= Port.Highlighted

handleNodeDragMouseUp :: MouseEvent -> NodeDrag -> Command State ()
handleNodeDragMouseUp evt nodeDrag = do
    coord <- workspacePosition evt
    let startPos = view nodeDragStartPos nodeDrag
        nl       = view nodeDragNodeLoc  nodeDrag
    if startPos == coord then
        selectNodes [nl]
    else do
        metaUpdate <- Map.fromList . fmap (view nodeLoc &&& view position)
            <$> getSelectedNodes
        localSetNodesMeta =<< toMetaUpdate metaUpdate
        case (nodeDrag ^. nodeDragSnappedConnId) of
            Just connId -> do
                mayConn <- getConnection connId
                withJust mayConn $ \conn ->
                    moveNodeOnConnection nl conn metaUpdate
            _           -> moveNodes metaUpdate
    continue stopNodeDrag


stopNodeDrag :: NodeDrag -> Command State ()
stopNodeDrag nodeDrag = do
    clearSnappedConnection nodeDrag
    removeActionFromState nodeDragAction
