module Luna.Studio.Action.Basic.RemovePort where

import           Control.Arrow
import qualified Data.Map.Lazy                             as Map
import           Empire.API.Data.Connection                (Connection (Connection), connectionId, src)
import           Empire.API.Data.Node                      (ports)
import           Empire.API.Data.Port                      (OutPort (Projection), PortId (OutPortId), portId)
import           Empire.API.Data.PortRef                   (AnyPortRef (OutPortRef'), OutPortRef (OutPortRef), srcPortId)
import           Luna.Studio.Action.Basic.AddConnection    (localAddConnection)
import           Luna.Studio.Action.Basic.RemoveConnection (removeConnection)
import           Luna.Studio.Action.Basic.UpdateNode       (localUpdateNode)
import qualified Luna.Studio.Action.Batch                  as Batch
import           Luna.Studio.Action.Command                (Command)
import           Luna.Studio.Action.State.Graph            (getConnectionsContainingNode)
import qualified Luna.Studio.Action.State.Graph            as Graph
import           Luna.Studio.Action.State.NodeEditor       (getNode)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Node              (hasPort, isInputEdge)
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
                let oldPorts    = Map.elems $ Map.delete (OutPortId pid) $ graphNode ^. ports
                    newPorts    = flip map oldPorts $ \port' -> case port' ^. portId of
                        OutPortId (Projection i) ->
                            if i < pos
                                then port'
                                else port' & portId .~ OutPortId (Projection (i-1))
                        _                        -> port'
                    newPortsMap = Map.fromList $ map (view portId &&& id) newPorts
                void . localUpdateNode $ graphNode & ports .~ newPortsMap
                conns <- getConnectionsContainingNode nid
                -- TODO[LJK]: Do it at once so we don't update the same Connection twice accidentaly
                forM_ conns $ \conn -> case conn of
                    Connection (OutPortRef srcNid (Projection i)) _ ->
                        when (srcNid == nid) $
                            if i == pos
                                then void . removeConnection   $ conn ^. connectionId
                            else if (i >= pos)
                                then void . localAddConnection $ conn & src . srcPortId .~ Projection (i-1)
                                else return ()
                    _ -> return ()
                return True
localRemovePort _ = $notImplemented
