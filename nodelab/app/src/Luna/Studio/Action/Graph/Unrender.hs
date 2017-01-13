{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.Action.Graph.Unrender
    ( unrender
    ) where

import           Luna.Studio.Prelude

import           Luna.Studio.Action.Command         (Command)
import qualified Luna.Studio.Batch.Workspace        as Workspace
import qualified Luna.Studio.React.Model.App        as App
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global


unrender :: Command State ()
unrender = do
    Global.workspace . Workspace.isGraphLoaded .= False
    Global.withApp $ do
        App.codeEditor .= def
        App.nodeEditor .= def
    Global.graph .= def
