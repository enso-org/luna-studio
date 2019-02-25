module Empire.Commands.Graph.Context
    ( module Empire.Commands.Graph.Context
    , module X
    ) where

import Empire.Commands.Library as X (withLibrary)

import Empire.Prelude

import qualified Data.Bimap as Bimap
import qualified Data.Map as Map
import qualified LunaStudio.Data.GraphLocation as GraphLocation
import qualified Luna.Package as Package
import qualified Luna.IR as IR
import qualified Empire.Data.BreadcrumbHierarchy as BH
import qualified Empire.Data.Graph as Graph
import qualified Empire.Commands.Library as Library
import qualified Empire.Data.Library as Library
import qualified Path

import Data.List (find)
-- import Empire.Commands.Graph.Breadcrumb (zoomBreadcrumb)
import Empire.Data.AST                  (astExceptionFromException,
                                         astExceptionToException)
import Empire.Data.Library (Library)
import Empire.Data.Graph                (ClsGraph, Graph)
import Empire.Empire                    (Command, Empire, activeFiles)
import LunaStudio.Data.Breadcrumb    (Breadcrumb(..), BreadcrumbItem(..), _Redirection)
import LunaStudio.Data.GraphLocation    (GraphLocation(..))

