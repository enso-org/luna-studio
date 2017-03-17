module Luna.Studio.Action.Basic.RemovePort where

import qualified Data.Map.Lazy                             as Map
import           Empire.API.Data.Node                      (ports)
import           Empire.API.Data.PortRef                   (AnyPortRef (OutPortRef'), OutPortRef (OutPortRef), srcPortId)
import           Luna.Studio.Action.Basic.AddConnection    (localAddConnection)
import           Luna.Studio.Action.Basic.RemoveConnection (removeConnection)
import           Luna.Studio.Action.Basic.UpdateNode       (localUpdateNode)
import qualified Luna.Studio.Action.Batch                  as Batch
import           Luna.Studio.Action.Command                (Command)
import qualified Luna.Studio.Action.State.Graph            as Graph
import           Luna.Studio.Action.State.NodeEditor       (getConnectionsContainingNode, getNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Connection        (connectionId, dst, src)
import           Luna.Studio.React.Model.Node              (hasPort, isInputEdge)
import           Luna.Studio.React.Model.Port              (OutPort (Projection), PortId (OutPortId), portId, toPortsMap)
import           Luna.Studio.State.Global                  (State)


removePort :: AnyPortRef -> Command State ()
removePort portRef = whenM (localRemovePort portRef) $ Batch.removePort portRef

localRemovePort :: AnyPortRef -> Command State Bool
localRemovePort (OutPortRef' (OutPortRef nid pid@(Projection pos))) = do
    mayNode      <- getNode nid
    mayGraphNode <- Graph.getNode nid
    flip (maybe (return False)) ((,) <$> mayNode <*> mayGraphNode) $ \(node, graphNode) ->
        if (not $ isInputEdge node) || hasPort (OutPortId pid) node
            then return False
            else do
                let oldPorts    = map convert . Map.elems $ Map.delete (OutPortId pid) $ graphNode ^. ports
                    newPorts    = flip map oldPorts $ \port' -> case port' ^. portId of
                        OutPortId (Projection i) ->
                            if i < pos
                                then port'
                                else port' & portId .~ OutPortId (Projection (i-1))
                        _                        -> port'
                    newPortsMap = toPortsMap newPorts
                void . localUpdateNode $ graphNode & ports .~ Map.map convert newPortsMap
                conns <- getConnectionsContainingNode nid
                -- TODO[LJK]: Do it at once so we don't update the same Connection twice accidentaly
                forM_ conns $ \conn -> case conn ^. src of
                    OutPortRef srcNid (Projection i) ->
                        when (srcNid == nid) $
                            if i == pos
                                then void . removeConnection   $ conn ^. connectionId
                            else if (i >= pos)
                                then void $ localAddConnection (conn ^. src & srcPortId .~ Projection (i-1)) (conn ^. dst)
                                else return ()
                    _ -> return ()
                return True
localRemovePort _ = $notImplemented
