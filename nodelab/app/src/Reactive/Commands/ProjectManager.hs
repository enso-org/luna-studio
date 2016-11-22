{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.ProjectManager
    ( loadProject
    , displayCurrentBreadcrumb
    , navigateToGraph
    , loadGraph
    ) where

import           Utils.PreludePlus

import qualified Batch.Workspace                  as Workspace
import qualified Reactive.Commands.Batch          as BatchCmd
import qualified Reactive.Commands.Breadcrumbs    as Breadcrumbs
import           Reactive.Commands.Command        (Command, performIO)
import           Reactive.Commands.Graph.Unrender (unrender)
import           Reactive.State.Global            (State)
import qualified Reactive.State.Global            as Global

import           Empire.API.Data.Breadcrumb       (Breadcrumb (..), BreadcrumbItem, Named)
import           Empire.API.Data.GraphLocation    (GraphLocation (..))
import qualified Empire.API.Data.GraphLocation    as GraphLocation
import           Empire.API.Data.Project          (ProjectId)

import qualified JS.GraphLocation                 as JS
import           UI.Instances                     ()


loadProject :: ProjectId -> Command State ()
loadProject projId = do
    let newLocation = GraphLocation projId 0 (Breadcrumb [])
    navigateToGraph newLocation

loadGraph :: GraphLocation -> Command State ()
loadGraph location = do
    unrender
    Global.workspace . Workspace.currentLocation .= location
    saveCurrentLocation
    BatchCmd.getProgram

navigateToGraph :: GraphLocation -> Command State ()
navigateToGraph location = do
    currentLocation <- use $ Global.workspace . Workspace.currentLocation
    when (currentLocation /= location) $ loadGraph location

displayCurrentBreadcrumb :: Breadcrumb (Named BreadcrumbItem) -> Command State ()
displayCurrentBreadcrumb breadcrumbs = Breadcrumbs.update enterBreadcrumbs breadcrumbs

enterBreadcrumbs :: Breadcrumb BreadcrumbItem -> Command State ()
enterBreadcrumbs newBc = do
    location <- use $ Global.workspace . Workspace.currentLocation
    let newLocation = location & GraphLocation.breadcrumb .~ newBc
    navigateToGraph newLocation

saveCurrentLocation :: Command State ()
saveCurrentLocation = do
    workspace <- use $ Global.workspace
    performIO $ JS.saveLocation $ workspace ^. Workspace.uiGraphLocation
