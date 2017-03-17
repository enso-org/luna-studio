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
import           Luna.Studio.React.Model.Node            (Node, NodeType (ExpressionNode))
import qualified Luna.Studio.React.Model.Node            as Node
import           Luna.Studio.React.Model.Port            (visible)
import           Luna.Studio.State.Global                (State)


addNode :: Position -> Text -> Command State ()
addNode nodePos expression = do
    selected <- getSelectedNodes
    nid      <- getUUID
    let nodeType    = ExpressionNode expression
        nodeMeta    = NodeMeta (toTuple $ snap nodePos) True
        connectTo   = if length selected == 1
                      then view Node.nodeId <$> listToMaybe selected
                      else Nothing
        defPortsMap = Map.fromList [ (InPortId  (Arg 0), Port (InPortId  (Arg 0)) "" TStar NotConnected) ,
                                     (OutPortId All    , Port (OutPortId All    ) "" TStar NotConnected) ]
        node        = convert $ Empire.Node nid def nodeType False defPortsMap nodeMeta def
    localAddNode node
    selectNode nid
    Batch.addNode nid expression nodeMeta connectTo
    GA.sendEvent $ GA.AddNode $ if isJust connectTo then GA.AutoConnect else GA.Simple

localAddNodes :: [Node] -> Command State ()
localAddNodes = mapM_ localAddNode

localAddNode :: Node -> Command State ()
localAddNode node = do
    selfPortVis <- shouldDisplayPortSelf $ node
    let node' = node & Node.ports . ix (InPortId Self) . visible .~ selfPortVis
    NodeEditor.addNode node'
    void . redrawConnectionsForNode $ node ^. Node.nodeId
    focusNode $ node ^. Node.nodeId
