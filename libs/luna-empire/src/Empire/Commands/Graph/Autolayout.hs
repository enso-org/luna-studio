module Empire.Commands.Graph.Autolayout where

import Empire.Prelude

import qualified Data.Map                        as Map
import qualified Empire.ASTOps.Read              as ASTRead
import qualified Empire.Commands.AST             as AST
import qualified Empire.Commands.Autolayout      as Autolayout
import qualified Empire.Data.BreadcrumbHierarchy as BH
import qualified Empire.Data.Graph               as Graph
import qualified LunaStudio.Data.Breadcrumb      as Breadcrumb
import qualified LunaStudio.Data.Position        as Position
import qualified Empire.Commands.GraphBuilder         as GraphBuilder

import Control.Lens                  (uses)
import Data.List                     (sortOn)
import Empire.ASTOp                  (runASTOp, ClassOp, GraphOp)
import Empire.Commands.Graph.Context (withGraph, withUnit, withBreadcrumb)
import Empire.Empire                 (Empire)
import LunaStudio.Data.Constants     (gapBetweenNodes)
import LunaStudio.Data.GraphLocation (GraphLocation, (|>))
import LunaStudio.Data.NodeId     (NodeId)
import Debug (timeIt, (<!!>))
import Empire.Commands.Graph.NodeMeta (setNodePositionAST, setNodePositionCls)


autolayout :: GraphLocation -> Empire ()
autolayout gl = do
    children <- withGraph gl $ runASTOp $ do
        children <- uses Graph.breadcrumbHierarchy (view BH.children)
        needLayout <- fmap catMaybes $ forM (Map.keys children) $ \nid -> do
            meta <- AST.getNodeMeta nid
            pure $ if meta /= def then Nothing else Just nid
        autolayoutNodesAST needLayout
        pure children
    let toBreadcrumb (k,v) = case v of
            BH.LambdaChild{}                -> [Breadcrumb.Lambda k]
            BH.ExprChild (BH.ExprItem pc _) -> map
                (Breadcrumb.Arg k)
                (Map.keys pc)
        next = concatMap toBreadcrumb $ Map.assocs children
    traverse_ (\a -> autolayout (gl |> a)) next

autolayoutNodes :: GraphLocation -> [NodeId] -> Empire ()
autolayoutNodes gl nids = withBreadcrumb
    gl
    (runASTOp $ autolayoutNodesAST nids)
    (runASTOp $ autolayoutNodesCls nids)

autolayoutTopLevel :: GraphLocation -> Empire ()
autolayoutTopLevel gl = withUnit gl $ runASTOp $ do
    clsFuns    <- use Graph.clsFuns
    needLayout <- fmap catMaybes $ forM (Map.assocs clsFuns) $ \(nid, fun) -> do
        f    <- ASTRead.getFunByNodeId nid
        meta <- AST.readMeta f
        let fileOffset = fun ^. Graph.funGraph . Graph.fileOffset
        pure $ if meta /= def then Nothing else Just (nid, fileOffset)

    let sortedNeedLayout = sortOn snd needLayout
        positions    = map (Position.fromTuple . (,0)) [0,gapBetweenNodes..]
        autolayouted = zipWith
            (\(nid, _) pos -> (nid,pos))
            sortedNeedLayout
            positions
    traverse_ (uncurry setNodePositionCls) autolayouted

autolayoutNodesAST :: [NodeId] -> GraphOp ()
autolayoutNodesAST nids = timeIt "autolayoutNodes" $ do
    nodes <- GraphBuilder.buildNodesForAutolayout <!!> "buildNodesForAutolayout"
    conns <- GraphBuilder.buildConnections        <!!> "buildConnections"
    traverse_
        (uncurry setNodePositionAST)
        (Autolayout.autolayoutNodes nids nodes conns) <!!> "setNodePositionsAST"

autolayoutNodesCls :: [NodeId] -> ClassOp ()
autolayoutNodesCls nids = GraphBuilder.buildNodesForAutolayoutCls >>= \nodes ->
    traverse_
        (uncurry setNodePositionCls) 
        $ Autolayout.autolayoutNodes nids nodes mempty
