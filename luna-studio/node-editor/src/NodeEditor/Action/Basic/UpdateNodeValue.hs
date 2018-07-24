{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.Action.Basic.UpdateNodeValue where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import qualified Data.Aeson                                 as Aeson
import qualified Data.Aeson.Encoding                        as Aeson
import qualified Data.Text                                  as Text
import qualified Data.Text.Lazy                             as Text.Lazy
import qualified Data.Text.Lazy.Encoding                    as Text.Lazy
import qualified IdentityString                             as IS
import           LunaStudio.Data.NodeValue                  (NodeValue (NodeError, NodeValue))
import           NodeEditor.Action.State.NodeEditor         (getExpressionNodeType, getVisualizersForType, modifyExpressionNode,
                                                             setVisualizationData)
import           NodeEditor.React.Model.Node.ExpressionNode (NodeLoc, Value (Error, ShortValue), value)
import           NodeEditor.React.Model.NodeEditor          (VisualizationBackup (ErrorBackup, MessageBackup, StreamBackup, ValueBackup))
import           NodeEditor.React.Model.Visualization       (VisualizationValue (StreamDataPoint, StreamStart, Value), noDataMsg, noVisMsg)
import           NodeEditor.State.Global                    (State)


encodeToLazyText :: Aeson.ToJSON a => a -> Text.Text
encodeToLazyText = Text.Lazy.toStrict . Text.Lazy.decodeUtf8 . Aeson.encodingToLazyByteString . Aeson.toEncoding

updateNodeValueAndVisualization :: NodeLoc -> NodeValue -> Command State ()
updateNodeValueAndVisualization nl = \case
    NodeValue sv (Just (StreamDataPoint visVal)) -> do
        modifyExpressionNode nl $ value .= ShortValue (Text.take 100 sv)
        !vis <- liftIO $ IS.fromJSString visVal
        setVisualizationData nl (StreamBackup [vis]) False
    NodeValue sv (Just (Value visVal)) -> do
        modifyExpressionNode nl $ value .= ShortValue (Text.take 100 sv)
        !vis <- liftIO $ IS.fromJSString visVal
        setVisualizationData nl (ValueBackup vis) True
    NodeValue sv (Just StreamStart) -> do
        modifyExpressionNode nl $ value .= ShortValue (Text.take 100 sv)
        setVisualizationData nl (StreamBackup []) True
    NodeValue sv Nothing -> do
        modifyExpressionNode nl $ value .= ShortValue (Text.take 100 sv)
        noVisualizers <- maybe (return False) (fmap isNothing . getVisualizersForType) =<< getExpressionNodeType nl
        let msg = if noVisualizers then noVisMsg else noDataMsg
        setVisualizationData nl (MessageBackup msg) True
    NodeError e -> do
        modifyExpressionNode nl $ value .= Error e
        setVisualizationData nl (ErrorBackup $ encodeToLazyText e) True
