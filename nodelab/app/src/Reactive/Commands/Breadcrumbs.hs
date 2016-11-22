{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Breadcrumbs (
    update
) where

import qualified Data.Text.Lazy               as Text
import           Utils.PreludePlus            hiding (group)
import           Utils.Vector                 (Vector2 (..), x)

import qualified Batch.Workspace              as Workspace
import qualified JS.UI                        as JS
import qualified Object.Widget.Button         as Button
import           Object.Widget.Icon           (Icon (..))
import           Reactive.Commands.Command    (Command)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (State, inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.UIElements    as UIElements
import           Reactive.State.UIRegistry    (addHandler)
import qualified Style.Layout                 as Style
import           UI.Handlers.Button           (ClickedHandler (..))
import           UI.Instances                 ()

import           Empire.API.Data.Breadcrumb   (Breadcrumb (..), BreadcrumbItem, Named)
import qualified Empire.API.Data.Breadcrumb   as Breadcrumb
import qualified Empire.API.Data.Project      as Project


destroyBreadcrumbs :: Command State ()
destroyBreadcrumbs = do
    breadcrumbs <- use $ Global.uiElements . UIElements.breadcrumbs
    breadcrumbItems <- inRegistry $ UICmd.children breadcrumbs
    inRegistry  $ mapM_ UICmd.removeWidget breadcrumbItems

displayBreadcrumbs :: (Breadcrumb BreadcrumbItem -> Command State ()) -> Breadcrumb (Named BreadcrumbItem) -> Command State ()
displayBreadcrumbs enterBreadcrumbs (Breadcrumb items) = do
    group <- use $ Global.uiElements . UIElements.breadcrumbs
    currentProjectName <- use $ Global.workspace . Workspace.currentProject . Project.name

    inRegistry $ do
        forM_ (reverse $ tails items) $ \bc -> do
            let name = case bc of
                    (item:_) -> case item of
                        Breadcrumb.Named name' _ -> name'
                    [] -> Text.pack currentProjectName
                widget = Button.create Style.breadcrumbItemSize name
                       & Button.style .~ Style.breadcrumbItemStyle
                       & Button.size  . x .~ (fromIntegral $ 5 + JS.calculateTextWidth name)
                unnamedBreadcrumbs = Breadcrumb $ map (^. Breadcrumb.breadcrumb) bc
                handlers = addHandler (ClickedHandler $ \_ -> enterBreadcrumbs unnamedBreadcrumbs) mempty
            when (length bc /= 0) $ UICmd.register_ group (Icon def (Vector2 20 20) "triangle") def
            UICmd.register group widget handlers


update :: (Breadcrumb BreadcrumbItem -> Command State ()) -> Breadcrumb (Named BreadcrumbItem)-> Command State ()
update enterBreadcrumbs breadcrumbs = do
    destroyBreadcrumbs
    displayBreadcrumbs enterBreadcrumbs breadcrumbs
