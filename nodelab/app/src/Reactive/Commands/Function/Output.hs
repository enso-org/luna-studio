module Reactive.Commands.Function.Output
    (registerOutput
    ) where

import           Prologue

import           Empire.API.Data.Node        (NodeId)
import           Empire.API.Data.Output      (Output)
import qualified Object.Widget.FunctionPort  as Model
import qualified React.Store                 as Store
import qualified React.Store.Function.Output as Output
import qualified React.Store.NodeEditor      as NodeEditor
import           Reactive.Commands.Command   (Command)
import           Reactive.State.Global       (State)
import qualified Reactive.State.Global       as Global
import qualified Reactive.State.Graph        as Graph
import           UI.Handlers.FunctionPort    ()



registerOutput :: NodeId -> Output -> Command State ()
registerOutput nodeId output = do
    let outputModel = Model.fromOutput nodeId output
    Global.inNodeEditor $ Store.modifyM_ $ do
        outputRef <- lift $ Store.create outputModel
        NodeEditor.outputs ?= outputRef
