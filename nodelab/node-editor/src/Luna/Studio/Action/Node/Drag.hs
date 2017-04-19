{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeApplications #-}
module Luna.Studio.Action.Node.Drag
    ( startNodeDrag
    , nodesDrag
    , handleNodeDragMouseUp
    ) where

import           Control.Arrow
import qualified Data.Map                                    as Map
import           Data.Position                               (Position, move, vector)
import           Empire.API.Data.NodeLoc                     (NodeLoc)
import           Empire.API.Data.Port                        (InPortIndex (Self))
import           Empire.API.Data.PortRef                     (InPortRef (InPortRef), OutPortRef (OutPortRef))
import           Luna.Studio.Action.Basic                    (connect, localMoveNodes, moveNodes, selectNodes, updatePortSelfVisibility)
import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.Node.Snap                (snap)
import           Luna.Studio.Action.State.Model              (createConnectionModel, getIntersectingConnections)
import           Luna.Studio.Action.State.NodeEditor         (getConnection, getExpressionNode, getNodeEditor, getSelectedNodes,
                                                              modifyConnection, modifyExpressionNode, modifyNodeEditor)
import           Luna.Studio.Event.Mouse                     (workspacePosition)
import           Luna.Prelude
import           Luna.Studio.React.Model.Connection          (Mode (Dimmed, Highlighted), dst, src)
import qualified Luna.Studio.React.Model.Connection          as Connection

import           Luna.Studio.React.Model.Node.ExpressionNode (inPortAt, isSelected, nodeLoc, position)
import           Luna.Studio.React.Model.NodeEditor          (halfConnections, toPosConnection)
import           Luna.Studio.React.Model.Port                (ensureVisibility, mode)
import           Luna.Studio.State.Action                    (Action (begin, continue, end, update), NodeDrag (NodeDrag), nodeDragAction,
                                                              nodeDragNodeLoc, nodeDragNodesStartPos, nodeDragSnappedConnIdAndPrevMode,
                                                              nodeDragStartPos)

import           Luna.Studio.Action.State.Action             (beginActionWithKey, continueActionWithKey, removeActionFromState,
                                                              updateActionWithKey)
import           Luna.Studio.State.Global                    (State)
import           React.Flux                                  (MouseEvent)


instance Action (Command State) NodeDrag where
    begin        = beginActionWithKey    nodeDragAction
    continue     = continueActionWithKey nodeDragAction
    update       = updateActionWithKey   nodeDragAction
    end nodeDrag = do
            metaUpdate <- map (view nodeLoc &&& view position) <$> getSelectedNodes
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
            void . localMoveNodes $ Map.toList snappedNodes
        else begin $ NodeDrag coord nl nodesPos Nothing

nodesDrag :: MouseEvent -> Bool -> NodeDrag -> Command State ()
nodesDrag evt snapped nodeDrag = do
    coord <- workspacePosition evt
    let mouseStartPos = view nodeDragStartPos      nodeDrag
        draggedNodeLoc = view nodeDragNodeLoc        nodeDrag
        nodesStartPos = view nodeDragNodesStartPos nodeDrag
        delta = coord ^. vector - mouseStartPos ^. vector
        shift' = if snapped then do
                     case Map.lookup draggedNodeLoc nodesStartPos of
                         Just pos -> snap (move delta pos) ^. vector - pos ^. vector
                         Nothing  -> delta
                 else delta
    void $ localMoveNodes . Map.toList $ Map.map (move shift') nodesStartPos
    snapConnectionsForNodes coord $ Map.keys nodesStartPos

clearSnappedConnection :: NodeDrag -> Command State ()
clearSnappedConnection nodeDrag = do
    let nl = nodeDrag ^. nodeDragNodeLoc
    modifyNodeEditor $ halfConnections .= def
    withJust (nodeDrag ^. nodeDragSnappedConnIdAndPrevMode) $ \(connId, m) ->
        modifyConnection connId $ Connection.mode .= m
    void $ updatePortSelfVisibility nl
    continue $ \nodeDrag' -> do
        update $ nodeDrag' & nodeDragSnappedConnIdAndPrevMode .~ Nothing

snapConnectionsForNodes :: Position -> [NodeLoc] -> Command State ()
snapConnectionsForNodes mousePos nodeLocs = when (length nodeLocs == 1) $ forM_ nodeLocs $ \nl -> do
    mayNode <- getExpressionNode nl
    withJust mayNode $ \node -> do
        mayConnId <- getIntersectingConnections node mousePos
        case mayConnId of
            Just connId -> do
                let selfPortRef = InPortRef  nl [Self]
                    outPortRef  = OutPortRef nl []
                mayConn       <- getConnection connId
                modifyExpressionNode nl $ inPortAt [Self] . mode %= ensureVisibility
                mayConnModel1 <- fmap join . mapM (flip createConnectionModel selfPortRef) $ view src <$> mayConn
                mayConnModel2 <- fmap join $ mapM (createConnectionModel outPortRef)       $ view dst <$> mayConn
                case (,,) <$> mayConn <*> mayConnModel1 <*> mayConnModel2 of
                    Just (conn, connModel1, connModel2) -> do
                        ne <- getNodeEditor
                        let conns = map (Connection.mode .~ Highlighted) [connModel1, connModel2]
                            conns' = mapMaybe (toPosConnection ne) conns
                        modifyNodeEditor $ halfConnections .= map convert conns'
                        continue $ \nodeDrag -> update $ nodeDrag & nodeDragSnappedConnIdAndPrevMode ?~ (connId, conn ^. Connection.mode)
                        modifyConnection connId $ Connection.mode .= Dimmed
                    _ -> continue clearSnappedConnection
            _ -> continue clearSnappedConnection

handleNodeDragMouseUp :: MouseEvent -> NodeDrag -> Command State ()
handleNodeDragMouseUp evt nodeDrag = do
    coord <- workspacePosition evt
    let startPos = view nodeDragStartPos nodeDrag
        nl       = view nodeDragNodeLoc  nodeDrag
    if startPos == coord then
        selectNodes [nl]
    else do
        metaUpdate <- map (view nodeLoc &&& view position) <$> getSelectedNodes
        moveNodes metaUpdate
        withJust (nodeDrag ^. nodeDragSnappedConnIdAndPrevMode) $ \(connId, _) -> do
            mayConn <- getConnection connId
            withJust mayConn $ \conn -> do
                connect (Left $ conn ^. src) $ Right nl
                connect (Right nl)           $ Left $ conn ^. dst
    continue stopNodeDrag


stopNodeDrag :: NodeDrag -> Command State ()
stopNodeDrag nodeDrag = do
    clearSnappedConnection nodeDrag
    removeActionFromState nodeDragAction
