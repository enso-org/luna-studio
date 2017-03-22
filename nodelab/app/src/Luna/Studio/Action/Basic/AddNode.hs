module Luna.Studio.Action.Basic.AddNode where

import qualified Data.Map.Lazy                           as Map
import           Data.Position                           (Position, toTuple)
import           Data.Text                               (Text)
import           Data.Vector                             ()
import qualified Empire.API.Data.Node                    as Empire
import           Empire.API.Data.NodeMeta                (NodeMeta (NodeMeta))
import           Empire.API.Data.Port                    (InPort (Arg, Self), OutPort (All), Port (Port), PortId (InPortId, OutPortId),
                                                          PortState (NotConnected))
import           Empire.API.Data.TypeRep                 (TypeRep (TStar))
import qualified JS.GoogleAnalytics                      as GA
import           Luna.Studio.Action.Basic.DrawConnection (redrawConnectionsForNode)
import           Luna.Studio.Action.Basic.FocusNode      (focusNode)
import           Luna.Studio.Action.Basic.SelectNode     (selectNode)
import qualified Luna.Studio.Action.Batch                as Batch
import           Luna.Studio.Action.Command              (Command)
import           Luna.Studio.Action.Node.Snap            (snap)
import           Luna.Studio.Action.State.Model          (shouldDisplayPortSelf)
import           Luna.Studio.Action.State.NodeEditor     (getSelectedNodes)
import qualified Luna.Studio.Action.State.NodeEditor     as NodeEditor
import           Luna.Studio.Action.UUID                 (getUUID)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node            (EdgeNode, ExpressionNode, Node (Edge, Expression), nodeId, ports)
import           Luna.Studio.React.Model.Port            (visible)
import           Luna.Studio.State.Global                (State)


addNode :: Position -> Text -> Command State ()
addNode nodePos expr = do
    selected <- getSelectedNodes
    nid      <- getUUID
    let nodeType    = Empire.ExpressionNode expr
        snappedPos  = snap nodePos
        nodeMeta    = NodeMeta (toTuple snappedPos) True
        connectTo   = if length selected == 1
                      then view nodeId <$> listToMaybe selected
                      else Nothing
        defPortsMap = Map.fromList [ (InPortId  (Arg 0), Port (InPortId  (Arg 0)) "" TStar NotConnected) ,
                                     (OutPortId All    , Port (OutPortId All    ) "" TStar NotConnected) ]
        node        = convert $ Empire.Node nid def nodeType False defPortsMap nodeMeta def
    localAddNode node
    selectNode nid
    Batch.addNode nid expr snappedPos True connectTo
    GA.sendEvent $ GA.AddNode $ if isJust connectTo then GA.AutoConnect else GA.Simple

localAddExpressionNodes :: [ExpressionNode] -> Command State ()
localAddExpressionNodes = mapM_ localAddExpressionNode

localAddNodes :: [Node] -> Command State ()
localAddNodes = mapM_ localAddNode

localAddNode :: Node -> Command State ()
localAddNode (Expression node) = localAddExpressionNode node
localAddNode (Edge edge)       = localAddEdgeNode edge

localAddEdgeNode :: EdgeNode -> Command State ()
localAddEdgeNode node = do
    NodeEditor.addEdgeNode node
    void . redrawConnectionsForNode $ node ^. nodeId

localAddExpressionNode :: ExpressionNode -> Command State ()
localAddExpressionNode node = do
    selfPortVis <- shouldDisplayPortSelf $ node
    let node' = node & ports . ix (InPortId Self) . visible .~ selfPortVis
    NodeEditor.addExpressionNode node'
    void . redrawConnectionsForNode $ node ^. nodeId
    focusNode $ node ^. nodeId
