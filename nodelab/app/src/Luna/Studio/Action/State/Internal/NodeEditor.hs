{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}
module Luna.Studio.Action.State.Internal.NodeEditor where

import qualified Control.Monad.State                         as M
import           Data.HashMap.Strict                         (HashMap)
import qualified Empire.API.Data.Breadcrumb                  as B
import qualified Empire.API.Data.NodeLoc                     as NodeLoc

import           Luna.Studio.Action.Command                  (Command)
import           Luna.Studio.Action.State.App                (modify)
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.App                 (nodeEditor)
import           Luna.Studio.React.Model.Node                (NodeId, NodeLoc)
import qualified Luna.Studio.React.Model.Node.ExpressionNode as ExpressionNode
import           Luna.Studio.React.Model.NodeEditor
import           Luna.Studio.State.Global                    (State)


addNodeRec :: Lens' NodeEditor (HashMap NodeId a) -> Lens' ExpressionNode.Subgraph (HashMap NodeId a) -> NodeLoc -> a -> Command State ()
addNodeRec rootLens subLens nl node = modifyNodeRec'
    (\nid -> modify nodeEditor $ rootLens . at nid ?= node)
    (\nid -> subLens . at nid ?= node)
    nl

removeNodeRec :: Lens' NodeEditor (HashMap NodeId a) -> Lens' ExpressionNode.Subgraph (HashMap NodeId a) -> NodeLoc -> Command State ()
removeNodeRec rootLens subLens nl = modifyNodeRec'
    (\nid -> modify nodeEditor $ rootLens . at nid .= Nothing)
    (\nid -> subLens . at nid .= Nothing)
    nl

modifyNodeRec :: Monoid r => Lens' NodeEditor (HashMap NodeId a) -> Lens' ExpressionNode.Subgraph (HashMap NodeId a) -> NodeLoc -> M.State a r -> Command State r
modifyNodeRec rootLens subLens nl st = modifyNodeRec'
    (\nid -> modify (nodeEditor . rootLens . at nid) $ zoom traverse st)
    (\nid -> zoom (subLens . at nid . traverse) st)
    nl

modifyNodeRec' :: Monoid r => (NodeId -> Command State r) -> (NodeId -> M.State ExpressionNode.Subgraph r) -> NodeLoc -> Command State r
modifyNodeRec' rootModify subModify nl = modifyNodeRec'' (nl ^. NodeLoc.pathItems) where
    modifyNodeRec'' []    = rootModify (nl ^. NodeLoc.nodeId)
    modifyNodeRec'' (h:t) = zoomRootNode (h ^. B.nodeId) $ zoomSubgraph h $ modifySubNode t

    modifySubNode []    = subModify (nl ^. NodeLoc.nodeId)
    modifySubNode (h:t) = zoomSubNode (h ^. B.nodeId) $ zoomSubgraph h $ modifySubNode t

    zoomSubNode  nid  = zoom (ExpressionNode.expressionNodes . at nid . traverse)
    zoomSubgraph item = zoom (ExpressionNode.subgraphs . at item . traverse)

    zoomRootNode nid = modify (nodeEditor . expressionNodes . at nid) . zoom traverse
