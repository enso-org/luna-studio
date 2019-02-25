module Empire.Commands.Graph.Metadata where

import Empire.Prelude hiding (link)

import qualified Data.Aeson                           as Aeson
import qualified Data.Aeson.Text                      as Aeson
import qualified Data.Graph.Data.Component.Vector     as PtrList
import qualified Data.Graph.Data.Layer.Class          as Layer
import qualified Data.Map                             as Map
import qualified Data.Text                            as Text
import qualified Data.Text.Encoding                   as Text
import qualified Data.Text.Lazy                       as TextLazy
import qualified Empire.ASTOps.Read                   as ASTRead
import qualified Empire.Commands.AST                  as AST
import qualified Empire.Commands.Code                 as Code
import qualified Empire.Data.BreadcrumbHierarchy      as BH
import qualified Empire.Data.Graph                    as Graph
import qualified Luna.IR                              as IR
import qualified Luna.Syntax.Text.Analysis.SpanTree   as SpanTree
import qualified Luna.Syntax.Text.Lexer               as Lexer
import qualified Luna.Syntax.Text.Parser.Ast.CodeSpan as CodeSpan
import qualified LunaStudio.Data.Breadcrumb           as Breadcrumb
import qualified LunaStudio.Data.GraphLocation        as GraphLocation
import qualified Safe

import Data.Text.Span                       (SpacedSpan (SpacedSpan))
import Empire.ASTOp                         (ASTOp, ClassOp, runASTOp)
import Empire.ASTOps.BreadcrumbHierarchy    (getMarker)
-- import Empire.Commands.Graph.Context        (withGraph, withUnit)
import Empire.Data.AST                      (EdgeRef, NodeRef)
import Empire.Data.FileMetadata             (FileMetadata (FileMetadata),
                                             MarkerNodeMeta (MarkerNodeMeta))
import Empire.Empire                        (Empire)
import Luna.Syntax.Text.Analysis.SpanTree   (Spanned (Spanned))
import Luna.Syntax.Text.Parser.Ast.CodeSpan (CodeSpan)
import LunaStudio.Data.Breadcrumb           (Breadcrumb (Breadcrumb))
import LunaStudio.Data.GraphLocation        (GraphLocation (GraphLocation))
import LunaStudio.Data.NodeCache            (NodeCache (NodeCache))
import LunaStudio.Data.NodeId               (NodeId)
import LunaStudio.Data.NodeMeta             (NodeMeta)


