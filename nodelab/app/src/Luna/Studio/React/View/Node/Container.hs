{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Node.Container where

import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.App  (App)
import           Luna.Studio.React.Model.Node (Subgraph)
import qualified Luna.Studio.React.Model.Node as Subgraph
import           Luna.Studio.React.Store      (Ref, dispatch)
import           Luna.Studio.React.View.Edge  (edgeSidebar_)
import           Luna.Studio.React.View.Monad (monad_)
import           React.Flux                   as React


name :: JSString
name = "node-container"

container_ :: Ref App -> [Subgraph] -> ReactElementM ViewEventHandler ()
container_ ref sgs = React.viewWithSKey container name (ref, sgs) mempty

container :: ReactView (Ref App, [Subgraph])
container = React.defineView name $ \(ref, sgs) -> do
    div_ $ elemString $ show sgs
