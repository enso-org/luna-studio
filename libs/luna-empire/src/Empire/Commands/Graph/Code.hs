module Empire.Commands.Graph.Code where

import Empire.Prelude hiding (range, span)

import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import qualified Empire.ASTOps.Parse           as ASTParse
import qualified Empire.ASTOps.Read            as ASTRead
import qualified Empire.Commands.Code          as Code
import qualified Empire.Commands.Publisher     as Publisher
import qualified Empire.Data.Graph             as Graph
import qualified Empire.Data.Library           as Library
import qualified Empire.Data.FileMetadata      as FileMetadata
import qualified Empire.Empire                 as Empire
import qualified LunaStudio.Data.GraphLocation as GraphLocation
import qualified Luna.Syntax.Text.Lexer        as Lexer

import Control.Monad.Catch                  (handle)
import Data.Char                            (isDigit, isSpace)
import Data.List                            (find, findIndices)
import Data.Text.Position                   (Delta (Delta))
import Empire.ASTOp                         (runASTOp)
import Empire.ASTOps.BreadcrumbHierarchy    (getMarker)
-- import Empire.Commands.Graph.Autolayout     (autolayout, autolayoutTopLevel)
-- import Empire.Commands.Graph.Breadcrumb     (makeGraph)
-- import Empire.Commands.Graph.Context        (withLibrary, withUnit)
-- import Empire.Commands.Graph.Metadata       (markFunctions, prepareNodeCache,
--                                              readMetadata', removeMetadataNode,
--                                              stripMetadata)
import Empire.Empire                        (Empire)
import Luna.Syntax.Text.Parser.State.Marker (TermMap (TermMap))
import LunaStudio.Data.Breadcrumb           (Breadcrumb (Breadcrumb),
                                             BreadcrumbItem (Definition))
import LunaStudio.Data.GraphLocation        (GraphLocation)
import LunaStudio.Data.NodeCache            (nodeIdMap, nodeMetaMap)
import LunaStudio.Data.Point                (Point)
import LunaStudio.Data.TextDiff             (TextDiff (TextDiff))
