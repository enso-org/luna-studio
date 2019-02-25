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

-- import Control.Lens                  (uses)
-- import Data.List                     (sortOn)
-- import Empire.ASTOp                  (runASTOp, ClassOp, GraphOp)
-- import Empire.Commands.Graph.Context (withGraph, withUnit, withBreadcrumb)
-- import Empire.Empire                 (Empire)
-- import LunaStudio.Data.Constants     (gapBetweenNodes)
-- import LunaStudio.Data.GraphLocation (GraphLocation, (|>))
-- import LunaStudio.Data.NodeId     (NodeId)
-- import Debug (timeIt, (<!!>))
-- import Empire.Commands.Graph.NodeMeta (setNodePositionAST, setNodePositionCls)

