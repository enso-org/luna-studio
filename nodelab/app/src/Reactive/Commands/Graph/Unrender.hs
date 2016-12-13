{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Graph.Unrender
    ( unrender
    ) where

import           Luna.Studio.Prelude

import qualified Batch.Workspace           as Workspace
import qualified React.Store               as Store
import qualified React.Model.NodeEditor    as NodeEditor
import           Reactive.Commands.Command (Command)
import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global



unrender :: Command State ()
unrender = do
    Global.workspace . Workspace.isGraphLoaded    .= False
    Global.withCodeEditor $ Store.modify_ $ const def
    Global.withNodeEditor $ Store.modify_ NodeEditor.reset
    Global.graph .= def
