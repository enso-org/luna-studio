module Reactive.Commands.Function.Input
    (registerInput
    ) where

import           Prologue

import qualified Empire.API.Data.Input      as API
import           Empire.API.Data.Node       (NodeId)
import qualified Object.Widget.FunctionPort as Model
import qualified React.Store                as Store
import qualified React.Store.Function.Input as Input
import qualified React.Store.NodeEditor     as NodeEditor
import           Reactive.Commands.Command  (Command)
import           Reactive.State.Global      (State)
import qualified Reactive.State.Global      as Global
import           UI.Handlers.FunctionPort   ()



registerInput :: NodeId -> Int -> API.Input -> Command State ()
registerInput nodeId inputNo input = do
    let inputModel = Model.fromInput nodeId input
    Global.inNodeEditor $ Store.modifyM_ $ do
        inputRef <- lift $ Store.create $ inputModel
        NodeEditor.inputs . at inputNo ?= inputRef
