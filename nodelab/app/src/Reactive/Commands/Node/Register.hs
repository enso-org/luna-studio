module Reactive.Commands.Node.Register
    ( registerNode
    ) where

import           Utils.PreludePlus
import           Utils.Vector                      (Vector2, toTuple)

import qualified Empire.API.Data.NodeMeta          as NodeMeta
import qualified JS.GoogleAnalytics                as GA
import qualified Object.Widget.Node                as UINode
import           React.Store                       (widget)
import qualified Reactive.Commands.Batch           as BatchCmd
import           Reactive.Commands.Command         (Command)
import           Reactive.Commands.Graph.Selection (selectedNodes)
import           Reactive.Commands.Node.Snap
import           Reactive.State.Global             (State)



registerNode :: Vector2 Int -> Text -> Command State ()
registerNode nodePos expr = do
    let nodePosSnapped = snap $ fmap fromIntegral nodePos
        nodeMeta = def & NodeMeta.position .~ toTuple nodePosSnapped
    selected   <- selectedNodes
    let connectTo = case selected of
            []     -> Nothing
            [wf]   -> Just $ wf ^. widget . UINode.nodeId
            (_:_) -> Nothing
    BatchCmd.addNode expr nodeMeta connectTo
    GA.sendEvent $ GA.AddNode $ if isJust connectTo then GA.AutoConnect else GA.Simple
