module Empire.Commands.Graph.NodeMeta where

import Empire.Prelude

import qualified Empire.ASTOps.Read       as ASTRead
import qualified Empire.Commands.AST      as AST
import qualified LunaStudio.Data.NodeMeta as NodeMeta

import Empire.ASTOp                  (ClassOp, GraphOp, runASTOp)
-- import Empire.Commands.Graph.Context (withBreadcrumb, withGraph)
import Empire.Data.Graph             (ClsGraph)
import Empire.Empire                 (Command, Empire)
import LunaStudio.Data.GraphLocation (GraphLocation)
import LunaStudio.Data.NodeId        (NodeId)
import LunaStudio.Data.NodeMeta      (NodeMeta)
import LunaStudio.Data.Position      (Position)


