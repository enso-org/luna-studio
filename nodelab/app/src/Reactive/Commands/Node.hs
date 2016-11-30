module Reactive.Commands.Node
    ( renameNode
    ) where

import           Utils.PreludePlus

import           Empire.API.Data.Node      (NodeId)
import qualified Empire.API.Data.Node      as Node
import qualified Object.Widget.Node        as Model
import qualified React.Store               as Store
import           Reactive.Commands.Command (Command)
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.Graph      as Graph



renameNode :: NodeId -> Text -> Command Global.State ()
renameNode nodeId name = do
    Global.graph . Graph.nodesMap . ix nodeId . Node.name .= name
    Global.inNode nodeId $
        mapM_ $ Store.modify_ (Model.name .~ name)
