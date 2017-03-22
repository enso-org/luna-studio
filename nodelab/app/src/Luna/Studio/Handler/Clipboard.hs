-- TODO[PM]: Finish implementation
module Luna.Studio.Handler.Clipboard where

import           Data.Aeson                          (decode, encode)
import           Data.ByteString.Lazy.Char8          (pack, unpack)
import qualified Data.HashMap.Strict                 as HashMap
import           Data.Position                       (x, y)
import qualified Data.Set                            as Set

import qualified Empire.API.Data.Connection          as Connection
import qualified Empire.API.Data.PortRef             as PortRef
import qualified JS.Clipboard                        as JS (copyStringToClipboard)
import           Luna.Studio.Action.Basic            (addSubgraph, removeSelectedNodes)
import           Luna.Studio.Action.Command          (Command)
import           Luna.Studio.Action.Node             (snapCoord)
import           Luna.Studio.Action.State.NodeEditor (getNodesMap, getSelectedNodes, separateSubgraph)
import           Luna.Studio.Action.State.Scene      (translateToWorkspace)
import qualified Luna.Studio.Data.Graph              as Graph
import           Luna.Studio.Event.Event             (Event (Shortcut))
import qualified Luna.Studio.Event.Shortcut          as Shortcut
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.EdgeNode    (EdgeNode)
import           Luna.Studio.React.Model.Node        (Node)
import qualified Luna.Studio.React.Model.Node        as Node
import           Luna.Studio.State.Global            (State)
import qualified Luna.Studio.State.Global            as Global
import qualified Luna.Studio.State.UI                as UI


handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event Shortcut.Paste (Just cbd))) = Just $ pasteFromClipboard cbd
handle (Shortcut (Shortcut.Event Shortcut.Copy   _        )) = Just copySelectionToClipboard
handle (Shortcut (Shortcut.Event Shortcut.Cut    _        )) = Just cutSelectionToClipboard
handle _ = Nothing

copySelectionToClipboard :: Command State ()
copySelectionToClipboard = do
    nodeIds  <- map (view Node.nodeId) <$> getSelectedNodes
    subgraph <- separateSubgraph nodeIds
    liftIO $ JS.copyStringToClipboard $ convert $ unpack $ encode subgraph

cutSelectionToClipboard :: Command State()
cutSelectionToClipboard = copySelectionToClipboard >> removeSelectedNodes

pasteFromClipboard :: String -> Command State ()
pasteFromClipboard clipboardData = do
    withJust (decode $ pack clipboardData) $ \subgraph -> do
        graphNodesIds <- Set.fromList . HashMap.keys <$> getNodesMap
        let nodes       = (convert <$> HashMap.elems (subgraph ^. Graph.nodesMap) :: [Either Node EdgeNode]) ^.. traverse . _Left
            connections = filter (\conn -> Set.member (conn ^. Connection.src . PortRef.srcNodeId) graphNodesIds) $ HashMap.elems $ subgraph ^. Graph.connectionsMap
        workspacePos <- translateToWorkspace =<< use (Global.ui . UI.mousePos)
        let shiftX = workspacePos ^. x - minimum (map (^. Node.position . x) nodes)
            shiftY = workspacePos ^. y - minimum (map (^. Node.position . y) nodes)
            shiftNode, shiftNodeX, shiftNodeY :: Node -> Node
            shiftNodeX = Node.position . x %~ snapCoord . (+shiftX)
            shiftNodeY = Node.position . y %~ snapCoord . (+shiftY)
            shiftNode = shiftNodeY . shiftNodeX
            nodes' = map shiftNode nodes
        --TODO[LJK]: Use unwrap here
        addSubgraph nodes' $ map (\conn -> (conn ^. Connection.src, conn ^. Connection.dst)) connections
