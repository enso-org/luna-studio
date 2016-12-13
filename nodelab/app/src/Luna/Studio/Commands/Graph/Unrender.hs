{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.Commands.Graph.Unrender
    ( unrender
    ) where

import           Luna.Studio.Prelude

import qualified Luna.Studio.Batch.Workspace           as Workspace
import qualified Luna.Studio.React.Store               as Store
import qualified Luna.Studio.React.Model.NodeEditor    as NodeEditor
import           Luna.Studio.Commands.Command (Command)
import           Luna.Studio.State.Global     (State)
import qualified Luna.Studio.State.Global     as Global



unrender :: Command State ()
unrender = do
    Global.workspace . Workspace.isGraphLoaded    .= False
    Global.withCodeEditor $ Store.modify_ $ const def
    Global.withNodeEditor $ Store.modify_ NodeEditor.reset
    Global.graph .= def
