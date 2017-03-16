module Luna.Studio.Handler.Clipboard where

import           Data.Aeson                          (decode, encode)
import           Data.ByteString.Lazy.Char8          (unpack)
import qualified Data.HashMap.Strict                 as HashMap
import           Data.Position                       (x, y)
import qualified Data.Set                            as Set
import           Data.Text.Encoding                  (encodeUtf8)

import qualified Empire.API.Data.Connection          as Connection
import           Empire.API.Data.Node                (Node)
import qualified Empire.API.Data.Node                as Node
import qualified Empire.API.Data.NodeMeta            as NodeMeta
import qualified Empire.API.Data.PortRef             as PortRef
import qualified JS.Clipboard                        as JS (copyStringToClipboard)
import           Luna.Studio.Action.Basic            (addSubgraph, removeSelectedNodes)
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.Node             (snapCoord)
import           Luna.Studio.Action.State.Graph      (getNodesMap, separateSubgraph)
import           Luna.Studio.Action.State.NodeEditor (getSelectedNodes)
import           Luna.Studio.Action.State.Scene      (translateToWorkspace)
import           Luna.Studio.Event.Event             (Event (Shortcut))
import qualified Luna.Studio.Event.Shortcut          as Shortcut
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node        as UINode
import           Luna.Studio.State.Global            (State)
import qualified Luna.Studio.State.Global            as Global
import           Luna.Studio.State.Graph             (Subgraph)
import qualified Luna.Studio.State.Graph             as Subgraph


handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event Shortcut.Paste (Just cbd))) = Just $ pasteFromClipboard cbd
handle (Shortcut (Shortcut.Event Shortcut.Copy   _        )) = Just copySelectionToClipboard
handle (Shortcut (Shortcut.Event Shortcut.Cut    _        )) = Just cutSelectionToClipboard
handle _ = Nothing

copySelectionToClipboard :: Command State ()
copySelectionToClipboard = do
    nodeIds  <- map (view UINode.nodeId) <$> getSelectedNodes
    subgraph <- separateSubgraph nodeIds
    liftIO $ JS.copyStringToClipboard $ convert $ unpack $ encode subgraph

cutSelectionToClipboard :: Command State()
cutSelectionToClipboard = copySelectionToClipboard >> removeSelectedNodes

pasteFromClipboard :: Text -> Command State ()
pasteFromClipboard clipboardData = do
  let maybeSubgraph = decode $ convert $ encodeUtf8 clipboardData :: Maybe Subgraph
  forM_ maybeSubgraph $ \subgraph -> do
      graphNodesIds <- Set.fromList . HashMap.keys <$> getNodesMap
      let nodes       = subgraph ^. Subgraph.nodesList
          connections = filter (\conn -> Set.member (conn ^. Connection.src . PortRef.srcNodeId) graphNodesIds) $ subgraph ^. Subgraph.connectionsList
      workspacePos <- translateToWorkspace =<< use Global.mousePos
      let shiftX = workspacePos ^. x - minimum (map (^. Node.nodeMeta . NodeMeta.position . _1) nodes)
          shiftY = workspacePos ^. y - minimum (map (^. Node.nodeMeta . NodeMeta.position . _2) nodes)
          shiftNode, shiftNodeX, shiftNodeY :: Node -> Node
          shiftNodeX = Node.nodeMeta . NodeMeta.position . _1 %~ snapCoord . (+shiftX)
          shiftNodeY = Node.nodeMeta . NodeMeta.position . _2 %~ snapCoord . (+shiftY)
          shiftNode = shiftNodeY . shiftNodeX
          nodes' = map shiftNode nodes
      addSubgraph nodes' connections
