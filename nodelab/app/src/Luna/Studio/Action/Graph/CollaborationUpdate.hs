module Luna.Studio.Action.Graph.CollaborationUpdate where

import qualified Data.DateTime                        as DT
import qualified Data.HashMap.Strict                  as HashMap
import qualified Data.Map.Lazy                        as Map
import           Empire.API.Data.GraphLocation        (GraphLocation)
import           Empire.API.Graph.CollaborationUpdate (ClientId)
import           Luna.Studio.Action.Batch             (collaborativeTouch)
import           Luna.Studio.Action.Command           (Command)
import           Luna.Studio.Action.Graph             (selectedNodes)
import qualified Luna.Studio.Batch.Workspace          as Workspace
import           Luna.Studio.Event.Batch              (Event (..))
import qualified Luna.Studio.Event.Event              as Event
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.Node         as NodeModel
import qualified Luna.Studio.React.Model.NodeEditor   as NodeEditor
import           Luna.Studio.State.Collaboration      (ColorId)
import           Luna.Studio.State.Collaboration      (ColorId)
import qualified Luna.Studio.State.Collaboration      as Collaboration
import           Luna.Studio.State.Global             (State)
import qualified Luna.Studio.State.Global             as Global


refreshTime, modifyTime :: Integer
refreshTime = 10
modifyTime  =  3

touchCurrentlySelected :: Command State ()
touchCurrentlySelected = do
    selected <- selectedNodes
    let nodeIds = view NodeModel.nodeId <$> selected
    collaborativeTouch nodeIds

expireTouchedNodes :: Command State ()
expireTouchedNodes = do
    currentTime <- use Global.lastEventTimestamp
    Global.modifyNodeEditor $ do
        let update = (  NodeModel.collaboration . NodeModel.touch  %~ Map.filter (\(ts, _) -> DT.diffSeconds ts currentTime > 0))
                     . (NodeModel.collaboration . NodeModel.modify %~ Map.filter (\ ts     -> DT.diffSeconds ts currentTime > 0))
        NodeEditor.nodes %= HashMap.map update

everyNSeconds :: Integer -> Command State () -> Command State ()
everyNSeconds interval action = do
    currentTime  <- use Global.lastEventTimestamp
    when (DT.toSeconds currentTime `mod` interval == 0) action

bumpTime :: DT.DateTime -> ColorId -> Maybe (DT.DateTime, ColorId) -> Maybe (DT.DateTime, ColorId)
bumpTime time color (Just (time', _)) = Just (max time time', color)
bumpTime time color Nothing           = Just (time, color)

updateClient :: ClientId -> Command State ColorId
updateClient clId = do
    mayCurrentData <- preuse $ Global.collaboration . Collaboration.knownClients . ix clId
    currentTime  <- use Global.lastEventTimestamp
    zoom Global.collaboration $ case mayCurrentData of
        Just currentData -> do
            Collaboration.knownClients . ix clId . Collaboration.lastSeen .= currentTime
            return $ currentData ^. Collaboration.colorId
        Nothing          -> do
            clients <- use Collaboration.knownClients
            let colors = Collaboration.unColorId . view Collaboration.colorId <$> Map.elems clients
                nextColor = case colors of
                    [] -> 0
                    _  -> maximum colors + 1
                nextColor' = Collaboration.ColorId nextColor
            Collaboration.knownClients . at clId ?= Collaboration.Client currentTime nextColor'
            return nextColor'
