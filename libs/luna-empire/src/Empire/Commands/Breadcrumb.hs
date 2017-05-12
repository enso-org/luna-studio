module Empire.Commands.Breadcrumb where

import           Empire.Prelude

import           Control.Monad.Except            (throwError)
import           Control.Monad.Reader            (ask)
import           Control.Monad.State             (get, put)
import           Data.Coerce                     (coerce)
import           Data.Maybe                      (maybe)

import           Empire.Data.BreadcrumbHierarchy (navigateTo, replaceAt)
import qualified Empire.Data.Graph               as Graph
import qualified Empire.Data.Library             as Library

import           LunaStudio.Data.Breadcrumb      (Breadcrumb (..), BreadcrumbItem (..))
import           LunaStudio.Data.Library         (LibraryId)
import           LunaStudio.Data.Node            (NodeId)
import           LunaStudio.Data.Project         (ProjectId)

import           Empire.Commands.Library         (withLibrary)
import           Empire.Empire                   (Command, Empire, runEmpire)

withBreadcrumb :: FilePath -> Breadcrumb BreadcrumbItem -> Command Graph.Graph a -> Empire a
withBreadcrumb file breadcrumb act = withLibrary file $ zoom Library.body $ zoomBreadcrumb breadcrumb act

zoomBreadcrumb :: Breadcrumb BreadcrumbItem -> Command Graph.Graph a -> Command Graph.Graph a
zoomBreadcrumb breadcrumb act = do
    graph <- get
    let  breadcrumbHierarchy = graph ^. Graph.breadcrumbHierarchy
    case breadcrumbHierarchy `navigateTo` breadcrumb of
        Just h -> do
            env <- ask
            let newGraph = graph & Graph.breadcrumbHierarchy .~ h
            (res, state) <- liftIO $ runEmpire env newGraph act
            case res of
                Right res' -> do
                    let modified = replaceAt breadcrumb breadcrumbHierarchy $ state ^. Graph.breadcrumbHierarchy
                    mod <- maybe (throwError $ show breadcrumb ++ " does not exist.") return modified
                    put $ state & Graph.breadcrumbHierarchy .~ mod
                    return res'
                Left err -> throwError err
        _ -> throwError $ show breadcrumb ++ " does not exist."
