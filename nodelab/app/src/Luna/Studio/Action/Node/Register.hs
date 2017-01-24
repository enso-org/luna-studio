module Luna.Studio.Action.Node.Register
    ( registerNode
    ) where

import           Data.Position                      (Position, toTuple, vector)
import qualified Empire.API.Data.NodeMeta           as NodeMeta
import qualified JS.GoogleAnalytics                 as GA
import qualified Luna.Studio.Action.Batch           as BatchCmd
import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Action.Graph.Selection (selectedNodes)
import           Luna.Studio.Action.Node.Snap
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node       as UINode
import           Luna.Studio.State.Global           (State)


registerNode :: Position -> Text -> Command State ()
registerNode nodePos expr = do
    let nodePosSnapped = snap nodePos
        nodeMeta = def & NodeMeta.position .~ toTuple (nodePosSnapped ^. vector)
    selected   <- selectedNodes
    let connectTo = case selected of
            []     -> Nothing
            [node] -> Just $ node ^. UINode.nodeId
            (_:_)  -> Nothing
    BatchCmd.addNode expr nodeMeta connectTo
    GA.sendEvent $ GA.AddNode $ if isJust connectTo then GA.AutoConnect else GA.Simple
