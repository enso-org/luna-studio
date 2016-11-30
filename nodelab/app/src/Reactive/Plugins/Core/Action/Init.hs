{-# LANGUAGE OverloadedStrings #-}
module Reactive.Plugins.Core.Action.Init where

import           Utils.PreludePlus                          hiding (group)
import           Utils.Vector                               (Vector2 (..))

import           Object.UITypes                             (WidgetId)
import qualified Object.Widget.Group                        as Group
import           Reactive.Commands.Command                  (Command)
import           Reactive.Commands.CommandSearcher.Commands (toggleText)
import qualified Reactive.Commands.UIRegistry               as UICmd
import           Reactive.State.Global                      (State, inRegistry)
import qualified Reactive.State.Global                      as Global
import qualified Reactive.State.UIElements                  as UIElements
import           Reactive.State.UIRegistry                  (addHandler, sceneInterfaceId)
import qualified Style.Layout                               as Style
import qualified UI.Handlers.Button                         as Button
import qualified UI.Layout                                  as Layout



initSidebar :: Command State WidgetId
initSidebar = do
    let group = Group.create & Group.style .~ Style.sidebar
                             & Group.size  .~ Vector2 Style.sidebarWidth 1000
    sidebar <- inRegistry $ UICmd.register sceneInterfaceId group def
    Global.uiElements . UIElements.sidebar .= sidebar
    return sidebar

initBreadcrumb :: Command State ()
initBreadcrumb = do
    let group = Group.create & Group.position  .~ Style.breadcrumbPosition
                             & Group.style     .~ Style.breadcrumbStyle
    groupId <- inRegistry $ UICmd.register sceneInterfaceId group $ Layout.horizontalLayoutHandlerNoResize 5.0
    Global.uiElements . UIElements.breadcrumbs .= groupId

initTextEditor :: Command State ()
initTextEditor = do
    let toggle = Style.textEditorToggle
    toggleId <- inRegistry $ UICmd.register sceneInterfaceId toggle $ addHandler (Button.ClickedHandler $ const $ toggleText) mempty

    Global.uiElements . UIElements.textEditorToggle .= toggleId

initialize :: Command State ()
initialize = do
    void $ initSidebar
    initBreadcrumb
    initTextEditor
