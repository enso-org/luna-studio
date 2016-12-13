{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Connection where

import           React.Flux
import qualified React.Flux             as React
import           Luna.Studio.Prelude

import qualified Event.UI               as UI
import           Luna.Studio.React.Store            (Ref, dt)
import qualified Luna.Studio.React.Store            as Store
import           Luna.Studio.React.Model.Connection (Connection)
import qualified Luna.Studio.React.Model.Connection as Connection


name :: JSString
name = "connection-editor"


connection :: Ref Connection -> ReactView ()
connection connectionRef = React.defineControllerView
    name connectionRef $ \connectionStore () -> do
        div_ $ do
            elemString $ "connection: " <> show (connectionStore ^. dt)


connection_ :: Ref Connection -> ReactElementM ViewEventHandler ()
connection_ connectionRef = React.view (connection connectionRef) () mempty
