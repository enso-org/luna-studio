module Reactive.Plugins.Core.Action.Clipboard where


import           Data.Aeson                        (decode, encode)
import           Data.ByteString.Lazy.Char8        (unpack)
import qualified Data.HashMap.Strict               as HashMap
import qualified Data.Set                          as Set
import           Data.Text.Lazy.Encoding           (encodeUtf8)
import qualified Empire.API.Data.Connection        as Connection
import           Empire.API.Data.Node              (Node)
import qualified Empire.API.Data.Node              as Node
import qualified Empire.API.Data.NodeMeta          as NodeMeta
import qualified Empire.API.Data.PortRef           as PortRef
import qualified Event.Clipboard                   as Clipboard
import           Event.Event                       (Event (..))
import           Event.Keyboard                    (KeyMods (..))
import qualified Event.Keyboard                    as Keyboard
import qualified Object.Widget                     as Widget
import qualified Object.Widget.Node                as UINode
import           Reactive.Commands.Batch           (addSubgraph)
import           Reactive.Commands.Command         (Command, performIO)
import           Reactive.Commands.Graph.Selection (selectedNodes)
import           Reactive.Commands.Node.Remove     (removeSelectedNodes)
import           Reactive.Commands.Node.Snap       (snapCoord)
import qualified Reactive.State.Camera             as Camera
import           Reactive.State.Global             (State)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.Graph              as Graph
import           Reactive.State.GraphSkeleton      as GraphSkeleton
import           Utils.PreludePlus
import           Utils.Vector                      (Vector2 (..))


foreign import javascript safe "clipboard.copy($1)" copyStringToClipboard :: JSString -> IO ()

toAction :: Event -> Maybe (Command State ())
-- TODO(LJK): Bind to event copy and cut instead of keyboard shortcut
toAction (Keyboard _ (Keyboard.Event Keyboard.Down 'C' (KeyMods False True False False))) = Just $ copySelectionToClipboard
toAction (Keyboard _ (Keyboard.Event Keyboard.Down 'X' (KeyMods False True False False))) = Just $ cutSelectionToClipboard
toAction (Clipboard (Clipboard.Paste clipboardData)) = Just $ pasteFromClipboard clipboardData
toAction _ = Nothing

copySelectionToClipboard :: Command State ()
copySelectionToClipboard = do
  nodeIds <- map (view $ Widget.widget . UINode.nodeId) <$> selectedNodes
  graph <- use Global.graph
  let subgraph = separateSubgraph nodeIds graph
  performIO $ copyStringToClipboard $ fromString $ unpack $ encode subgraph

cutSelectionToClipboard :: Command State()
cutSelectionToClipboard = copySelectionToClipboard >> removeSelectedNodes

pasteFromClipboard :: Text -> Command State ()
pasteFromClipboard clipboardData = do
  let maybeSkeleton = decode $ encodeUtf8 clipboardData :: Maybe GraphSkeleton
  when (isJust maybeSkeleton) $ do
      graphNodesIds <- Set.fromList . HashMap.keys <$> use (Global.graph . Graph.nodesMap)
      let skeleton    = fromJust maybeSkeleton
          nodes       = view GraphSkeleton.nodesList skeleton
          connections = filter (\conn -> Set.member (conn ^. Connection.src . PortRef.srcNodeId) graphNodesIds) $ view GraphSkeleton.connectionsList skeleton
      mousePos <- use $ Global.mousePos
      Vector2 mousePosX mousePosY <- zoom Global.camera $ Camera.screenToWorkspaceM mousePos
      let shiftX = mousePosX - (minimum $ map (^. Node.nodeMeta . NodeMeta.position . _1) nodes)
          shiftY = mousePosY - (minimum $ map (^. Node.nodeMeta . NodeMeta.position . _2) nodes)
          shiftNodeX :: Node -> Node
          shiftNodeX = Node.nodeMeta . NodeMeta.position . _1 %~ snapCoord . (+shiftX)
          shiftNodeY :: Node -> Node
          shiftNodeY = Node.nodeMeta . NodeMeta.position . _2 %~ snapCoord . (+shiftY)
          shiftNode :: Node -> Node
          shiftNode = shiftNodeY . shiftNodeX
          nodes' = map shiftNode nodes
      addSubgraph nodes' connections
