{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Node.Container where

import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.App  (App)
import           Luna.Studio.React.Model.Node (Subgraph)
import           Luna.Studio.React.Store      (Ref)
import           React.Flux                   as React


name :: JSString
name = "node-container"

container_ :: Ref App -> [Subgraph] -> ReactElementM ViewEventHandler ()
container_ ref sgs = React.viewWithSKey container name (ref, sgs) mempty

container :: ReactView (Ref App, [Subgraph])
container = React.defineView name $ \(_ref, sgs) -> do
    div_ $ elemString $ show sgs
