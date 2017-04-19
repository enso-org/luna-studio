{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Node.Editor.React.View.Breadcrumbs (
    breadcrumbs,
    breadcrumbs_
) where

import qualified Node.Editor.Event.UI                as UI
import           Luna.Prelude
import           Node.Editor.React.Model.App         (App)
import           Node.Editor.React.Model.Breadcrumbs (Breadcrumbs)
import qualified Node.Editor.React.Model.Breadcrumbs as B
import           Node.Editor.React.Store             (Ref, dispatch)
import qualified Node.Editor.React.View.Style       as Style
import           React.Flux
import qualified React.Flux                          as React


name :: JSString
name = "breadcrumbs"

breadcrumbs :: ReactView (Ref App, Breadcrumbs)
breadcrumbs = React.defineView name $ \(ref, model) ->
    div_
        [ "className" $= Style.prefixFromList [ "breadcrumbs", "noselect" ]
        , "key"       $=  name
        ] $
        forKeyed_ (inits $ model ^. B.items) $ \(key, bc) ->
            div_
                [ "className" $= Style.prefixFromList ["breadcrumbs__item", "breadcrumbs__item--home"]
                , "key"       $= jsShow key
                , onClick $ \_ _ -> dispatch ref $ UI.BreadcrumbsEvent $ B.Enter $ unname bc
                ] $ case reverse bc of
                    []       -> elemString "default"
                    (item:_) -> elemString $ convert $ item ^. B.name

breadcrumbs_ :: Ref App -> Breadcrumbs -> ReactElementM ViewEventHandler ()
breadcrumbs_ ref model = React.viewWithSKey breadcrumbs name (ref, model) mempty

unname :: [B.Named a] -> B.Breadcrumb a
unname = B.Breadcrumb . map B._breadcrumb
