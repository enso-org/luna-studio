module NodeEditor.Action.Basic.AddConnection where

import           Common.Action.Command              (Command)
import           Common.Prelude
import           Control.Monad                      (filterM)
import           LunaStudio.Data.PortRef            (AnyPortRef (InPortRef', OutPortRef'), InPortRef, OutPortRef)
import qualified NodeEditor.Action.Batch            as Batch
import           NodeEditor.Action.State.Model      (updatePortMode)
import           NodeEditor.Action.State.Model      (createConnectionModel)
import qualified NodeEditor.Action.State.NodeEditor as NodeEditor
import           NodeEditor.React.Model.Connection  (ConnectionId)
import           NodeEditor.React.Model.Node        (NodeLoc)
import           NodeEditor.State.Global            (State)


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
        updatePortMode $ OutPortRef' src'
        updatePortMode $ InPortRef'  dst'
    return $ isJust mayConn
