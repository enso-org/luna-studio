module Luna.Studio.Handler.Clipboard where

import           Data.Aeson                           (decode, encode)
import           Data.ByteString.Lazy.Char8           (unpack)
import qualified Data.HashMap.Strict                  as HashMap
import           Data.Position                        (x, y)
import qualified Data.Set                             as Set
import           Data.Text.Encoding                   (encodeUtf8)

import qualified Empire.API.Data.Connection           as Connection
import           Empire.API.Data.Node                 (Node)
import qualified Empire.API.Data.Node                 as Node
import qualified Empire.API.Data.NodeMeta             as NodeMeta
import qualified Empire.API.Data.PortRef              as PortRef
import qualified JS.Clipboard                         as JS (copyStringToClipboard)
import qualified Luna.Studio.Action.Camera            as Camera
import           Luna.Studio.Action.Command           (Command)
import           Luna.Studio.Action.Graph             (selectedNodes)
import           Luna.Studio.Action.Graph.AddSubgraph (addSubgraph)
import           Luna.Studio.Action.Node              (removeSelectedNodes, snapCoord)
import           Luna.Studio.Event.Event              (Event (Shortcut))
import qualified Luna.Studio.Event.Shortcut           as Shortcut
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node         as UINode
import           Luna.Studio.State.Global             (State)
import qualified Luna.Studio.State.Global             as Global
import qualified Luna.Studio.State.Graph              as Graph
import           Luna.Studio.State.GraphSkeleton      as GraphSkeleton


handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event Shortcut.Paste (Just cbd))) = Just $ pasteFromClipboard cbd
handle (Shortcut (Shortcut.Event Shortcut.Copy   _        )) = Just copySelectionToClipboard
handle (Shortcut (Shortcut.Event Shortcut.Cut    _        )) = Just cutSelectionToClipboard
handle _ = Nothing

copySelectionToClipboard :: Command State ()
copySelectionToClipboard = do
    nodeIds <- map (view UINode.nodeId) <$> selectedNodes
    graph   <- use Global.graph
    let subgraph = separateSubgraph nodeIds graph
    liftIO $ JS.copyStringToClipboard $ convert $ unpack $ encode subgraph

cutSelectionToClipboard :: Command State()
cutSelectionToClipboard = copySelectionToClipboard >> removeSelectedNodes

pasteFromClipboard :: Text -> Command State ()
pasteFromClipboard clipboardData = do
  let maybeSkeleton = decode $ convert $ encodeUtf8 clipboardData :: Maybe GraphSkeleton
  forM_ maybeSkeleton $ \skeleton -> do
      graphNodesIds <- Set.fromList . HashMap.keys <$> use (Global.graph . Graph.nodesMap)
      let nodes       = skeleton ^. GraphSkeleton.nodesList
          connections = filter (\conn -> Set.member (conn ^. Connection.src . PortRef.srcNodeId) graphNodesIds) $ skeleton ^. GraphSkeleton.connectionsList
      workspacePos <- Camera.translateToWorkspace =<< use Global.mousePos
      let shiftX = workspacePos ^. x - minimum (map (^. Node.nodeMeta . NodeMeta.position . _1) nodes)
          shiftY = workspacePos ^. y - minimum (map (^. Node.nodeMeta . NodeMeta.position . _2) nodes)
          shiftNode, shiftNodeX, shiftNodeY :: Node -> Node
          shiftNodeX = Node.nodeMeta . NodeMeta.position . _1 %~ snapCoord . (+shiftX)
          shiftNodeY = Node.nodeMeta . NodeMeta.position . _2 %~ snapCoord . (+shiftY)
          shiftNode = shiftNodeY . shiftNodeX
          nodes' = map shiftNode nodes
      addSubgraph nodes' connections
