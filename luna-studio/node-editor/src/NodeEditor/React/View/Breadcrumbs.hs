{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module NodeEditor.React.View.Breadcrumbs (
    breadcrumbs,
    breadcrumbs_
) where

import qualified NodeEditor.Event.UI                as UI
import           Common.Prelude
import           NodeEditor.React.Model.App         (App)
import           NodeEditor.React.Model.Breadcrumbs (Breadcrumbs)
import qualified NodeEditor.React.Model.Breadcrumbs as B
import           NodeEditor.React.Store             (Ref, dispatch)
import qualified NodeEditor.React.View.Style       as Style
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
