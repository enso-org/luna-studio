{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Luna.Studio.React.View.Breadcrumbs (
    breadcrumbs,
    breadcrumbs_
) where

import qualified Luna.Studio.Event.UI                as UI
import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.App         (App)
import           Luna.Studio.React.Model.Breadcrumbs (Breadcrumbs)
import qualified Luna.Studio.React.Model.Breadcrumbs as B
import           Luna.Studio.React.Store             (Ref, dispatch)
import           React.Flux
import qualified React.Flux                          as React


name :: JSString
name = "breadcrumbs"

breadcrumbs :: ReactView (Ref App, Breadcrumbs)
breadcrumbs = React.defineView name $ \(ref, model) -> do
    div_
        [ "className" $= (name <> "luna-noselect")
        , "key"       $=  name
        ] $ do
        forM_ (zip [0..] $ inits $ model ^. B.items) $ \(key, bc) -> do
            div_
                [ "className" $= "luna-breadcrumbs__item luna-breadcrumbs__item--home"
                , "key"       $= jsShow key
                , onClick $ \_ _ -> dispatch ref $ UI.BreadcrumbsEvent $ B.Enter $ unname bc
                ] $ case reverse bc of
                    []       -> elemString "default"
                    (item:_) -> elemString $ convert $ item ^. B.name

breadcrumbs_ :: Ref App -> Breadcrumbs -> ReactElementM ViewEventHandler ()
breadcrumbs_ ref model = React.viewWithSKey breadcrumbs name (ref, model) mempty

unname :: [B.Named a] -> B.Breadcrumb a
unname = B.Breadcrumb . map B._breadcrumb
