module Empire.Commands.Breadcrumb (
      withBreadcrumb
      ) where

import           Prologue                   hiding (at, toList)

import           Control.Monad.Except       (throwError)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Coerce                (coerce)
import           Data.Foldable
import           Data.List                  (find, delete)
import           Data.Tree

import           Empire.Data.BreadcrumbHierarchy (navigateTo, replaceAt)
import qualified Empire.Data.Graph          as Graph
import qualified Empire.Data.Library        as Library

import           Empire.API.Data.Breadcrumb (Breadcrumb (..), BreadcrumbItem (..))
import           Empire.API.Data.Library    (LibraryId)
import           Empire.API.Data.Node       (NodeId)
import           Empire.API.Data.Project    (ProjectId)

import           Empire.Commands.Library    (withLibrary)
import           Empire.Empire              (Command, Empire, empire, runEmpire)

withBreadcrumb :: ProjectId -> LibraryId -> Breadcrumb BreadcrumbItem -> Command Graph.Graph a -> Empire a
withBreadcrumb pid lid breadcrumb act = withLibrary pid lid $
    zoom (Library.body) $ do
        graph <- get
        let  breadcrumbHierarchy = graph ^. Graph.breadcrumbHierarchy
        case breadcrumbHierarchy `navigateTo` breadcrumb of
            Just newHierarchy -> do
                env <- ask
                let newGraph = graph & Graph.breadcrumbHierarchy .~ newHierarchy
                                     & Graph.insideNode .~ lastBreadcrumb breadcrumb
                (res, state) <- liftIO $ runEmpire env newGraph act
                case res of
                    Right res' -> do
                        let modifiedHierarchy = state ^. Graph.breadcrumbHierarchy
                        properHierarchy <- case replaceAt breadcrumb modifiedHierarchy breadcrumbHierarchy of
                            Just x -> return x
                            _      -> throwError $ show breadcrumb ++ " does not exist."
                        let properState = state & Graph.breadcrumbHierarchy .~ properHierarchy
                                                & Graph.insideNode .~ Nothing
                        put properState
                        return res'
                    Left err -> throwError err
            _ -> throwError $ show breadcrumb ++ " does not exist."

lastBreadcrumb :: Breadcrumb BreadcrumbItem -> Maybe NodeId
lastBreadcrumb breadcrumb = case coerce breadcrumb of
    [] -> Nothing
    breadcrumbs -> Just $ (\(Lambda nid) -> nid) . last $ breadcrumbs
