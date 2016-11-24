{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Graph.Unrender
    ( unrender
    ) where

import           Utils.PreludePlus

import qualified React.Store                  as Store
import qualified React.Store.App              as App
import qualified React.Store.NodeEditor       as NodeEditor
import           Reactive.Commands.Command    (Command, performIO)
import           Reactive.State.Global        (State, inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.Graph         as Graph

import qualified Batch.Workspace              as Workspace

import qualified JS.TextEditor                as UI
import           Reactive.Commands.UIRegistry (removeWidget)
import           UI.Instances                 ()



unrender :: Command State ()
unrender = do
    connWidgets   <- use $ Global.graph . Graph.connectionWidgets
    inputWidgets  <- use $ Global.graph . Graph.inputWidgets
    outputWidget  <- use $ Global.graph . Graph.outputWidget

    let allWidgetIds = connWidgets ++ inputWidgets ++ maybeToList outputWidget
    inRegistry $ mapM_ removeWidget allWidgetIds

    Global.graph     . Graph.connectionWidgetsMap .= def
    Global.graph     . Graph.inputWidgetsMap      .= def
    Global.graph     . Graph.outputWidget         .= def
    Global.workspace . Workspace.isGraphLoaded    .= False
    Global.inApp $ Store.modifyM_ $ \app -> do
        nodeEditor <- NodeEditor.create
        return $ app & App.nodeEditor .~ nodeEditor

    performIO $ UI.setText ""

    Global.graph .= def
