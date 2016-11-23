module Reactive.Commands.Function.Input
    (registerInput
    ) where

import           Prologue

import           Empire.API.Data.Input      (Input)
import           Empire.API.Data.Node       (NodeId)
import qualified Object.Widget.FunctionPort as Model
import qualified React.Store                as Store
import qualified React.Store.Function.Input as Input
import qualified React.Store.NodeEditor     as NodeEditor
import           Reactive.Commands.Command  (Command)
import           Reactive.State.Global      (State)
import qualified Reactive.State.Global      as Global
import           UI.Handlers.FunctionPort   ()



registerInput :: NodeId -> Int -> Input -> Command State ()
registerInput nodeId inputNo input = do
    let inputModel = Model.fromInput nodeId input
    Global.inNodeEditor $ Store.modify_ $ \ nodeEditor -> do
        inputRef <- Input.create inputModel
        return $ nodeEditor & NodeEditor.inputs . at inputNo ?~ inputRef
