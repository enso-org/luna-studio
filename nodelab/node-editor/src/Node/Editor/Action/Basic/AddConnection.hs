module Node.Editor.Action.Basic.AddConnection where

import           Control.Monad                       (filterM)
import           Empire.API.Data.PortRef             (AnyPortRef (InPortRef'), InPortRef, OutPortRef, dstNodeLoc, dstPortId)
import           Node.Editor.Action.Basic.UpdateNode (updatePortSelfVisibility)
import qualified Node.Editor.Action.Batch            as Batch
import           Node.Editor.Action.Command          (Command)
import           Node.Editor.Action.State.Model      (createConnectionModel)
import qualified Node.Editor.Action.State.NodeEditor as NodeEditor
import           Luna.Prelude
import           Node.Editor.React.Model.Connection  (ConnectionId)
import           Node.Editor.React.Model.Node        (NodeLoc)
import           Node.Editor.React.Model.Port        (InPortIndex (Self))
import           Node.Editor.State.Global            (State)


connect :: Either OutPortRef NodeLoc -> Either InPortRef NodeLoc -> Command State ()
connect src'@(Left srcPortRef) (Left dstPortRef) =
    whenM (localAddConnection srcPortRef dstPortRef) $ Batch.addConnection src' (Left $ InPortRef' dstPortRef)
connect src' (Left dstPortRef) = Batch.addConnection src' (Left $ InPortRef' dstPortRef)
connect src' (Right nid)       = Batch.addConnection src' (Right nid)

localAddConnections :: [(OutPortRef, InPortRef)] -> Command State [ConnectionId]
localAddConnections = (fmap . map) snd . filterM (uncurry localAddConnection)

localAddConnection :: OutPortRef -> InPortRef -> Command State Bool
localAddConnection src' dst' = do
    mayConn <- createConnectionModel src' dst'
    withJust mayConn $ \conn -> do
        NodeEditor.addConnection conn
        when ([Self] == dst' ^. dstPortId) . void . updatePortSelfVisibility $ dst' ^. dstNodeLoc
    return $ isJust mayConn
