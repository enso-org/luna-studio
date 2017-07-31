{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}
module NodeEditor.Action.State.Internal.NodeEditor where

import qualified Control.Monad.State                        as M
import           Data.HashMap.Strict                        (HashMap)
import qualified LunaStudio.Data.Breadcrumb                 as B
import qualified LunaStudio.Data.NodeLoc                    as NodeLoc

import           Common.Action.Command                      (Command)
import           Common.Prelude
import           NodeEditor.Action.State.App                (modify)
import           NodeEditor.React.Model.App                 (nodeEditor)
import           NodeEditor.React.Model.Node                (HasNodeLoc, NodeId, NodeLoc, nodeId)
import qualified NodeEditor.React.Model.Node.ExpressionNode as ExpressionNode
import           NodeEditor.React.Model.NodeEditor
import           NodeEditor.State.Global                    (State)


addNodeRec :: Lens' NodeEditor (HashMap NodeId n) -> Lens' ExpressionNode.Subgraph (HashMap NodeId n) -> NodeLoc -> n -> Command State ()
addNodeRec rootLens subLens nl node = modifyNodeRec'
    (\nid -> modify nodeEditor $ rootLens . at nid ?= node)
    (\nid -> subLens . at nid ?= node)
    nl

setNodeRec :: Lens' NodeEditor (Maybe n) -> Lens' ExpressionNode.Subgraph (Maybe n) -> NodeLoc -> n -> Command State ()
setNodeRec rootLens subLens nl node = modifyNodeRec'
    (\_ -> modify nodeEditor $ rootLens ?= node)
    (\_ -> subLens ?= node)
    nl

removeNodeRec :: Lens' NodeEditor (HashMap NodeId n) -> Lens' ExpressionNode.Subgraph (HashMap NodeId n) -> NodeLoc -> Command State ()
removeNodeRec rootLens subLens nl = modifyNodeRec'
    (\nid -> modify nodeEditor $ rootLens . at nid .= Nothing)
    (\nid -> subLens . at nid .= Nothing)
    nl

modifyNodeRec :: Monoid r => Lens' NodeEditor (HashMap NodeId n) -> Lens' ExpressionNode.Subgraph (HashMap NodeId n) -> NodeLoc -> M.State n r -> Command State r
modifyNodeRec rootLens subLens nl st = modifyNodeRec'
    (\nid -> modify (nodeEditor . rootLens . at nid) $ zoom traverse st)
    (\nid -> zoom (subLens . at nid . traverse) st)
    nl

modifySidebarRec :: (HasNodeLoc n, Monoid r) => Lens' NodeEditor (Maybe n) -> Lens' ExpressionNode.Subgraph (Maybe n) -> NodeLoc -> M.State n r -> Command State r
modifySidebarRec rootLens subLens nl st = modifyNodeRec'
    (\nid -> modify (nodeEditor . rootLens . filtered ((== Just nid) . fmap (view nodeId))) $ zoom traverse st)
    (\nid -> zoom (subLens . filtered ((== Just nid) . fmap (view nodeId)) . traverse) st)
    nl

modifyNodeRec' :: Monoid r => (NodeId -> Command State r) -> (NodeId -> M.State ExpressionNode.Subgraph r) -> NodeLoc -> Command State r
modifyNodeRec' rootModify subModify nl = modifyNodeRec'' (nl ^. NodeLoc.pathItems) where
    modifyNodeRec'' []    = rootModify (nl ^. nodeId)
    modifyNodeRec'' (h:t) = zoomRootNode (h ^. B.nodeId) $ zoomSubgraph h $ modifySubNode t

    modifySubNode []    = subModify (nl ^. nodeId)
    modifySubNode (h:t) = zoomSubNode (h ^. B.nodeId) $ zoomSubgraph h $ modifySubNode t

    zoomSubNode  nid  = zoom (ExpressionNode.expressionNodes . at nid . traverse)
    zoomSubgraph item = zoom (ExpressionNode.subgraphs . at item . traverse)

    zoomRootNode nid = modify (nodeEditor . expressionNodes . at nid) . zoom traverse
