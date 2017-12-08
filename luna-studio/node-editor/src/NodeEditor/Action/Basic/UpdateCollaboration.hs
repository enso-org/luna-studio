module NodeEditor.Action.Basic.UpdateCollaboration where

import           Common.Action.Command                      (Command)
import           Common.Prelude
import qualified Control.Monad.State                        as M
import qualified Data.DateTime                              as DT
import qualified Data.Map.Lazy                              as Map
import           LunaStudio.API.Graph.CollaborationUpdate   (ClientId)
import qualified NodeEditor.Action.Batch                    as Batch
import           NodeEditor.Action.State.NodeEditor         (getSelectedNodes, modifyExpressionNodes_)
import           NodeEditor.Data.Color                      (Color (Color), fromColor)
import           NodeEditor.React.Model.Node.ExpressionNode (modify, nodeLoc, touch)
import qualified NodeEditor.React.Model.Node.ExpressionNode as Node
import           NodeEditor.State.Collaboration             (Client (Client), colorId, knownClients, lastSeen)
import           NodeEditor.State.Global                    (State, collaboration)

updateCollaboration :: Command State ()
updateCollaboration = expireTouchedNodes >> everyNSeconds refreshTime touchCurrentlySelected

updateClient :: ClientId -> Command State Color
updateClient clId = do
    mayCurrentData <- use $ collaboration . knownClients . at clId
    currentTime    <- liftIO DT.getCurrentTime
    zoom collaboration $ case mayCurrentData of
        Just currentData -> do
            knownClients . ix clId . lastSeen .= currentTime
            return $ currentData ^. colorId
        Nothing          -> do
            colors <- map (fromColor . view colorId) <$> Map.elems <$> use knownClients
            let nextColor = Color $ if null colors then 0 else maximum colors + 1
            knownClients . at clId ?= Client currentTime nextColor
            return nextColor

refreshTime, modifyTime :: Integer
refreshTime = 10
modifyTime  =  3

touchCurrentlySelected :: Command State ()
touchCurrentlySelected = (map (view nodeLoc) <$> getSelectedNodes) >>= Batch.collaborativeTouch

expireTouchedNodes :: Command State ()
expireTouchedNodes = do
    currentTime  <- liftIO DT.getCurrentTime
    let update = ( Node.collaboration . touch  %~ Map.filter (\(ts, _) -> DT.diffSeconds ts currentTime > 0) )
               . ( Node.collaboration . modify %~ Map.filter (\ ts     -> DT.diffSeconds ts currentTime > 0) )
    modifyExpressionNodes_ $ M.modify update

everyNSeconds :: Integer -> Command State () -> Command State ()
everyNSeconds interval action = liftIO DT.getCurrentTime >>= \currentTime ->
    when (DT.toSeconds currentTime `mod` interval == 0) action

bumpTime :: DT.DateTime -> Color -> Maybe (DT.DateTime, Color) -> Maybe (DT.DateTime, Color)
bumpTime time color (Just (time', _)) = Just (max time time', color)
bumpTime time color Nothing           = Just (time, color)
