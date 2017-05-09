-- TODO[PM]: Finish implementation
module NodeEditor.Handler.Clipboard where

import           Data.Aeson                                 (decode, encode)
import           Data.ByteString.Lazy.Char8                 (pack, unpack)
import qualified Data.HashMap.Strict                        as HashMap
import qualified Data.Set                                   as Set
import           Empire.API.Data.Position                   (x, y)

import           Common.Prelude
import qualified Empire.API.Data.Connection                 as Connection
import qualified Empire.API.Data.GraphLocation              as GraphLocation
import           Empire.API.Data.NodeLoc                    (NodePath (NodePath))
import qualified Empire.API.Data.PortRef                    as PortRef
import qualified JS.Clipboard                               as JS (copyStringToClipboard)
import           NodeEditor.Action.Basic                    (addSubgraph, removeSelectedNodes)
import           NodeEditor.Action.Command                  (Command)
import           NodeEditor.Action.Node                     (snapCoord)
import           NodeEditor.Action.State.NodeEditor         (getExpressionNodes, getSelectedNodes, separateSubgraph)
import           NodeEditor.Action.State.Scene              (translateToWorkspace)
import           NodeEditor.Batch.Workspace                 (currentLocation)
import qualified NodeEditor.Data.Graph                      as Graph
import           NodeEditor.Event.Event                     (Event (Shortcut))
import qualified NodeEditor.Event.Shortcut                  as Shortcut
import           NodeEditor.React.Model.Node                (ExpressionNode)
import           NodeEditor.React.Model.Node.ExpressionNode (nodeLoc, position)
import           NodeEditor.State.Global                    (State, workspace)
import qualified NodeEditor.State.Global                    as Global
import qualified NodeEditor.State.UI                        as UI


handle :: Event -> Maybe (Command State ())
handle (Shortcut (Shortcut.Event Shortcut.Paste (Just cbd))) = Just $ pasteFromClipboard cbd
handle (Shortcut (Shortcut.Event Shortcut.Copy   _        )) = Just copySelectionToClipboard
handle (Shortcut (Shortcut.Event Shortcut.Cut    _        )) = Just cutSelectionToClipboard
handle _ = Nothing

copySelectionToClipboard :: Command State ()
copySelectionToClipboard = do
    nodeLocs  <- map (view nodeLoc) <$> getSelectedNodes
    subgraph <- separateSubgraph nodeLocs
    liftIO $ JS.copyStringToClipboard $ convert $ unpack $ encode subgraph

cutSelectionToClipboard :: Command State()
cutSelectionToClipboard = copySelectionToClipboard >> removeSelectedNodes

pasteFromClipboard :: String -> Command State ()
pasteFromClipboard clipboardData =
    withJust (decode $ pack clipboardData) $ \subgraph ->
        withJustM (preuse (workspace . traverse . currentLocation . GraphLocation.breadcrumb)) $ \selectedBc -> do --FIXME
            graphNodesLocs <- Set.fromList . map (view nodeLoc)  <$> getExpressionNodes
            let nodes       = (convert . (NodePath selectedBc,) <$> HashMap.elems (subgraph ^. Graph.nodesMap))
                connections = filter (\conn -> Set.member (conn ^. Connection.src . PortRef.srcNodeLoc) graphNodesLocs) $ HashMap.elems $ subgraph ^. Graph.connectionsMap
            workspacePos <- translateToWorkspace =<< use (Global.ui . UI.mousePos)
            let shiftX = workspacePos ^. x - minimum (map (^. position . x) nodes)
                shiftY = workspacePos ^. y - minimum (map (^. position . y) nodes)
                shiftNode, shiftNodeX, shiftNodeY :: ExpressionNode -> ExpressionNode
                shiftNodeX = position . x %~ snapCoord . (+shiftX)
                shiftNodeY = position . y %~ snapCoord . (+shiftY)
                shiftNode = shiftNodeY . shiftNodeX
                nodes' = map shiftNode nodes
            --TODO[LJK]: Use unwrap here
            addSubgraph nodes' $ convert <$> connections
