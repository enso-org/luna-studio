module Reactive.Commands.Node.Register
    ( registerNode
    ) where

import           Utils.PreludePlus
import           Utils.Vector                      (toTuple)

import           Reactive.Commands.Command         (Command)
import           Reactive.State.Global             (State)
import qualified Reactive.State.Global             as Global

import           Object.Widget                     (widget)
import qualified Object.Widget.Node                as UINode
import qualified Reactive.Commands.Batch           as BatchCmd
import           Reactive.Commands.Graph.Selection (selectedNodes)
import qualified Reactive.State.UIElements         as UIElements

import qualified Empire.API.Data.NodeMeta          as NodeMeta
import qualified JS.GoogleAnalytics                as GA

import          Reactive.Commands.Node.Snap


registerNode :: Text -> Command State ()
registerNode expr = do
    nodePos <- use $ Global.uiElements . UIElements.nsPos
    let nodePosSnapped = snap nodePos
        nodeMeta = def & NodeMeta.position .~ (toTuple nodePosSnapped)
    selected   <- selectedNodes
    let connectTo = case selected of
            []     -> Nothing
            [wf]   -> Just $ wf ^. widget . UINode.nodeId
            (_:_) -> Nothing
    BatchCmd.addNode expr nodeMeta connectTo
    GA.sendEvent $ GA.AddNode $ if isJust connectTo then GA.AutoConnect else GA.Simple
