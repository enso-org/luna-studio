{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module NodeEditor.React.View.Breadcrumbs (
    breadcrumbs,
    breadcrumbs_
) where

import           Common.Prelude
import qualified NodeEditor.Event.UI                as UI
import           NodeEditor.React.IsRef             (IsRef, dispatch)
import           NodeEditor.React.Model.Breadcrumbs (Breadcrumbs)
import qualified NodeEditor.React.Model.Breadcrumbs as B
import qualified NodeEditor.React.View.Style        as Style
import           React.Flux
import qualified React.Flux                         as React


name :: JSString
name = "breadcrumbs"

breadcrumbs :: IsRef r => ReactView (r, Maybe String, Breadcrumbs)
breadcrumbs = React.defineView name $ \(ref, moduleName, bcs) ->
    div_
        [ "className" $= Style.prefixFromList [ "breadcrumbs", "noselect" ]
        , "key"       $=  name
        ] $
        forKeyed_ (inits $ bcs ^. B.items) $ \(key, bc) ->
            div_
                [ "className" $= Style.prefixFromList ["breadcrumbs__item", "breadcrumbs__item--home"]
                , "key"       $= jsShow key
                , onClick $ \_ _ -> dispatch ref $ UI.BreadcrumbsEvent $ B.Enter $ unname bc
                ] $ case reverse bc of
                    []       -> elemString $ fromMaybe "No file selected" moduleName
                    (item:_) -> elemString $ convert $ item ^. B.name

breadcrumbs_ :: IsRef r => r -> Maybe String -> Breadcrumbs -> ReactElementM ViewEventHandler ()
breadcrumbs_ ref moduleName bcs = React.viewWithSKey breadcrumbs name (ref, moduleName, bcs) mempty

unname :: [B.Named a] -> B.Breadcrumb a
unname = B.Breadcrumb . map B._breadcrumb
