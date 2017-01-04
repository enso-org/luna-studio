{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Luna.Studio.React.View.Breadcrumbs (
    breadcrumbs,
    breadcrumbs_
) where

import           Data.Text.Lazy          (unpack)
import           React.Flux
import qualified React.Flux              as React
import           Luna.Studio.Prelude

import qualified Event.UI                as UI
import           Luna.Studio.React.Store             (Ref, dispatch, dt)
import           Luna.Studio.React.Model.Breadcrumbs (Breadcrumbs)
import qualified Luna.Studio.React.Model.Breadcrumbs as B



name :: JSString
name = "breadcrumbs"


breadcrumbs :: Ref Breadcrumbs -> ReactView ()
breadcrumbs ref = React.defineControllerView name ref $ \store () -> do
    div_ [ "className" $= name
         , "key"       $= name ] $ do
        forM_ (zip [0..] $ inits $ store ^. dt . B.items) $ \(key, bc) -> do
            div_
                [ "className" $= "breadcrumbs__item breadcrumbs__item--home"
                , "key"       $= fromString (show key)
                , onClick $ \_ _ -> dispatch ref $ UI.BreadcrumbsEvent $ B.Enter $ unname bc
                ] $ case reverse bc of
                    []       -> elemString "default"
                    (item:_) -> elemString $ unpack $ item ^. B.name


breadcrumbs_ :: Ref Breadcrumbs -> ReactElementM ViewEventHandler ()
breadcrumbs_ ref = React.viewWithSKey (breadcrumbs ref) name () mempty

unname :: [B.Named a] -> B.Breadcrumb a
unname = B.Breadcrumb . map B._breadcrumb

--TODO[react]
-- initBreadcrumb :: Command State ()
-- initBreadcrumb = do
--     let group = Group.create & Group.position  .~ Style.breadcrumbPosition
--                              & Group.style     .~ Style.breadcrumbStyle
--     groupId <- inRegistry $ UICmd.register sceneInterfaceId group $ Layout.horizontalLayoutHandlerNoResize 5.0
--     Global.uiElements . UIElements.breadcrumbs .= groupId


-- displayBreadcrumbs :: (Breadcrumb BreadcrumbItem -> Command State ()) -> Breadcrumb (Named BreadcrumbItem) -> Command State ()
-- displayBreadcrumbs enterBreadcrumbs (Breadcrumb items) = do
--     group <- use $ Global.uiElements . UIElements.breadcrumbs
--     currentProjectName <- use $ Global.workspace . Workspace.currentProject . Project.name
--
--     inRegistry $ do
--         forM_ (reverse $ tails items) $ \bc -> do
--             let name = case bc of
--                     (item:_) -> case item of
--                         Breadcrumb.Named name' _ -> name'
--                     [] -> Text.pack currentProjectName
--                 widget = Button.create Style.breadcrumbItemSize name
--                        & Button.style .~ Style.breadcrumbItemStyle
--                        & Button.size  . x .~ (fromIntegral $ 5 + JS.calculateTextWidth name)
--                 unnamedBreadcrumbs = Breadcrumb $ map (^. Breadcrumb.breadcrumb) bc
--                 handlers = addHandler (ClickedHandler $ \_ -> enterBreadcrumbs unnamedBreadcrumbs) mempty --TODO[react] handle enterBreadcrumbs
--             when (length bc /= 0) $ UICmd.register_ group (Icon def (Vector2 20 20) "triangle") def
--             UICmd.register group widget handlers
