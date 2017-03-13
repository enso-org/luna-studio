{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Node.Case where

import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.App  (App)
import           Luna.Studio.React.Model.Node (SubGraph)
import qualified Luna.Studio.React.Model.Node as SubGraph
import           Luna.Studio.React.Store      (Ref, dispatch)
import           Luna.Studio.React.View.Edge  (edgeSidebar_)
import           Luna.Studio.React.View.Monad (monad_)
import           React.Flux                   as React


name :: JSString
name = "node-case"

expandedCase_ :: Ref App -> [SubGraph] -> ReactElementM ViewEventHandler ()
expandedCase_ ref sgs = React.viewWithSKey expandedCase name (ref, sgs) mempty

expandedCase :: ReactView (Ref App, [SubGraph])
expandedCase = React.defineView name $ \(ref, sgs) -> do
    div_ $ elemString $ show sgs
