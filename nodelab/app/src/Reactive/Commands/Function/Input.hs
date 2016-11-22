module Reactive.Commands.Function.Input
    (registerInput
    ) where

import           Prologue

import           Empire.API.Data.Input        (Input)
import           Empire.API.Data.Node         (NodeId)
import qualified Object.Widget.FunctionPort   as Model
import qualified React.Stores                 as Stores
import           Reactive.Commands.Command    (Command)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (State, inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.Graph         as Graph
import qualified Reactive.State.UIElements    as UIElements
import           UI.Handlers.FunctionPort     ()



registerInput :: NodeId -> Int -> Input -> Command State ()
registerInput nodeId inputNo input = do
    let inputModel = Model.fromInput nodeId input
    inputRef <- Input.create inputModel
    Global.stores . Stores.inputs . at inputNo ?= inputWidget
