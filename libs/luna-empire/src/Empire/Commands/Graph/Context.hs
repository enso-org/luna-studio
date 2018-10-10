module Empire.Commands.Graph.Context
    ( module Empire.Commands.Graph.Context
    , module X
    ) where

import Empire.Commands.Library as X (withLibrary)

import Empire.Prelude

import qualified LunaStudio.Data.GraphLocation as GraphLocation

import Empire.Commands.Graph.Breadcrumb (zoomBreadcrumb)
import Empire.Data.AST                  (astExceptionFromException,
                                         astExceptionToException)
import Empire.Data.Graph                (ClsGraph, Graph)
import Empire.Empire                    (Command, Empire)
import LunaStudio.Data.GraphLocation    (GraphLocation)


data UnsupportedOperation = UnsupportedOperation deriving Show

instance Exception UnsupportedOperation where
    fromException = astExceptionFromException
    toException   = astExceptionToException

withGraph :: GraphLocation -> Command Graph a -> Empire a
withGraph gl act = withBreadcrumb gl act (throwM UnsupportedOperation)

withUnit :: GraphLocation -> Command ClsGraph a -> Empire a
withUnit gl act = withBreadcrumb gl (throwM UnsupportedOperation) act

withBreadcrumb
    :: GraphLocation -> Command Graph a -> Command ClsGraph a -> Empire a
withBreadcrumb gl = withLibrary (gl ^. GraphLocation.filePath)
    .: zoomBreadcrumb (gl ^. GraphLocation.breadcrumb)
