{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Field where

import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.App (App)
import           Luna.Studio.React.Store     (Ref, dispatch)
import           React.Flux
import qualified React.Flux                  as React


name :: JSString
name = "field"

data Mode = Disabled
          | Single
          | MultiLine
          deriving (Eq)

field :: [PropertyOrHandler ViewEventHandler] -> ReactView (Ref App, Mode, Text)
field ph = React.defineView name $ \(ref, mode, content) -> case mode of
    Disabled  -> div_ $ elemString $ convert content
    MultiLine -> textarea_ ph $ elemString $ convert content
    Single    -> input_ ("value" $= convert content : ph)

field_ :: [PropertyOrHandler ViewEventHandler] -> Mode -> Ref App -> JSString -> Text -> ReactElementM ViewEventHandler ()
field_ ph mode ref key  content = React.viewWithSKey (field ph) key (ref, mode, content) mempty

multilineField_ :: [PropertyOrHandler ViewEventHandler] -> Ref App -> JSString -> Text -> ReactElementM ViewEventHandler ()
multilineField_ = field_ MultiLine
