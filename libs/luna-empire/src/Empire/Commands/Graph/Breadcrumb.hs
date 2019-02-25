module Empire.Commands.Graph.Breadcrumb where

import Empire.Prelude

import qualified Data.Map                             as Map
import qualified Data.UUID.V4                         as UUID
import qualified Empire.ASTOps.BreadcrumbHierarchy    as ASTBreadcrumb
import qualified Empire.ASTOps.Read                   as ASTRead
import qualified Empire.Data.BreadcrumbHierarchy      as BH
import qualified Empire.Data.Graph                    as Graph
import qualified Empire.Data.Library                  as Library
import qualified Luna.Syntax.Text.Parser.Ast.CodeSpan as CodeSpan

import Control.Exception.Safe          (handle)
import Control.Lens                    (Prism')
import Control.Monad                   (forM)
import Control.Monad.Reader            (ask)
import Control.Monad.State             (get, put)
import Data.Maybe                      (listToMaybe, maybe)
import Data.Text.Span                  (SpacedSpan (SpacedSpan))
import Empire.ASTOp                    (GraphOp, runASTOp, runAliasAnalysis)
import Empire.Commands.Code            (functionBlockStartRef, propagateLengths)
import Empire.Data.AST                 (NodeRef)
import Empire.Data.BreadcrumbHierarchy (navigateTo, replaceAt)
import Empire.Data.Layers              (Marker, SpanLength)
import Empire.Empire                   (Command, runEmpire, zoomCommand)
import LunaStudio.Data.Breadcrumb      (Breadcrumb (Breadcrumb),
                                        BreadcrumbItem (Definition))
import LunaStudio.Data.NodeCache       (portMappingMap)
import LunaStudio.Data.NodeId          (NodeId)
import LunaStudio.Data.PortRef         (OutPortRef (OutPortRef))
