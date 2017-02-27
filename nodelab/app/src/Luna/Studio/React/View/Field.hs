{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Field where

import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.App (App)
import           Luna.Studio.React.Store     (Ref, dispatch)
import           React.Flux
import qualified React.Flux                  as React


name :: JSString
name = "field"

field :: ReactView (Ref App, Bool, Text)
field = React.defineView name $ \(ref, editable, content) -> do
    if editable then
        textarea_ [ onClick $ \e _ -> [stopPropagation e]] $
            elemString $ convert content
    else
        div_ $ elemString $ convert content

field_ :: Ref App -> JSString -> Bool -> Text -> ReactElementM ViewEventHandler ()
field_ ref key editable content = React.viewWithSKey field key (ref, editable, content) mempty
