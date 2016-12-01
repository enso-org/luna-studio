{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Graph.Unrender
    ( unrender
    ) where

import           Utils.PreludePlus

import qualified Batch.Workspace           as Workspace
import qualified React.Store               as Store
import qualified React.Store.App           as App
import           Reactive.Commands.Command (Command)
import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global



unrender :: Command State ()
unrender = do
    Global.workspace . Workspace.isGraphLoaded    .= False
    Global.withApp $ Store.modifyM_ $ do
        nodeEditor <- lift $ Store.create def
        codeEditor <- lift $ Store.create def
        App.nodeEditor .= nodeEditor
        App.codeEditor .= codeEditor
    Global.graph .= def
