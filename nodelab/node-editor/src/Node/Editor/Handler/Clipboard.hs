-- TODO[PM]: Finish implementation
module Node.Editor.Handler.Clipboard where

import           Data.Aeson                                  (decode, encode)
import           Data.ByteString.Lazy.Char8                  (pack, unpack)
import qualified Data.HashMap.Strict                         as HashMap
import           Data.Position                               (x, y)
import qualified Data.Set                                    as Set

import qualified Empire.API.Data.Connection                  as Connection
import qualified Empire.API.Data.GraphLocation               as GraphLocation
import           Empire.API.Data.NodeLoc                     (NodePath (NodePath))
import qualified Empire.API.Data.PortRef                     as PortRef
import qualified JS.Clipboard                                as JS (copyStringToClipboard)
import           Node.Editor.Action.Basic                    (addSubgraph, removeSelectedNodes)
import           Node.Editor.Action.Command                  (Command)
import           Node.Editor.Action.Node                     (snapCoord)
import           Node.Editor.Action.State.NodeEditor         (getExpressionNodes, getSelectedNodes, separateSubgraph)
import           Node.Editor.Action.State.Scene              (translateToWorkspace)
import           Node.Editor.Batch.Workspace                 (currentLocation)
import qualified Node.Editor.Data.Graph                      as Graph
import           Node.Editor.Event.Event                     (Event (Shortcut))
import qualified Node.Editor.Event.Shortcut                  as Shortcut
import           Luna.Prelude
import           Node.Editor.React.Model.Node                (ExpressionNode)
import           Node.Editor.React.Model.Node.ExpressionNode (nodeLoc, position)
import           Node.Editor.State.Global                    (State, workspace)
import qualified Node.Editor.State.Global                    as Global
import qualified Node.Editor.State.UI                        as UI


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
pasteFromClipboard clipboardData = do
    withJust (decode $ pack clipboardData) $ \subgraph -> do
        selectedBc <- use (workspace . currentLocation . GraphLocation.breadcrumb) --FIXME
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
