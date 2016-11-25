{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Graph.Unrender
    ( unrender
    ) where

import           Utils.PreludePlus

import qualified Batch.Workspace           as Workspace
import qualified React.Store               as Store
import qualified React.Store.App           as App
import qualified React.Store.CodeEditor    as CodeEditor
import qualified React.Store.NodeEditor    as NodeEditor
import           Reactive.Commands.Command (Command)
import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global



unrender :: Command State ()
unrender = do
    Global.workspace . Workspace.isGraphLoaded    .= False
    Global.inApp $ Store.modifyM_ $ \app -> do
        nodeEditor <- NodeEditor.create
        codeEditor <- CodeEditor.create
        return $ app & App.nodeEditor .~ nodeEditor
                     & App.codeEditor .~ codeEditor
    Global.graph .= def
