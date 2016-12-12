{-# LANGUAGE OverloadedStrings #-}
module React.View.Visualization where

import qualified Data.Text.Lazy                       as Text

import           Empire.API.Graph.NodeResultUpdate    (NodeValue)
import qualified Empire.API.Graph.NodeResultUpdate    as NodeResult
import           React.Flux
import qualified React.Flux                           as React
import           React.Store.Node                     (Node)
import qualified React.Store.Node                     as Node
import           Reactive.Commands.Node.Visualization (limitString, showError)
import           Utils.PreludePlus


name :: JSString
name = "visualization"


visualization :: ReactView NodeValue
visualization = React.defineView name $ \vis -> do
    text_ $ elemString "visualization"


visualization_ :: NodeValue -> ReactElementM ViewEventHandler ()
visualization_ v = React.view visualization v mempty



strValue :: Node -> String
strValue n = Text.unpack $ case n ^. Node.value of
    Nothing -> ""
    Just (NodeResult.Value value []) -> value
    Just (NodeResult.Value value valueReprs) -> value
        -- visualizeNodeValueReprs widgetId valueReprs --TODO[react]
    Just (NodeResult.Error msg) ->
        limitString errorLen (Text.pack $ showError msg)
        -- . (Model.isError .~ True)
        -- visualizeError widgetId msg --TODO[react]

errorLen :: Int
errorLen = 40
