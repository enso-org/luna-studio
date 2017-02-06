{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.Action.Collaboration
    ( updateClient
    ) where

import qualified Data.Map.Lazy                   as Map
import           Empire.API.Graph.Collaboration  (ClientId)
import           Luna.Studio.Action.Command      (Command)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Collaboration (ColorId)
import qualified Luna.Studio.State.Collaboration as Collaboration
import           Luna.Studio.State.Global        (State)
import qualified Luna.Studio.State.Global        as Global


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
            let colors = Collaboration.unColorId . (view Collaboration.colorId) <$> Map.elems clients
                nextColor = case colors of
                    [] -> 0
                    _  -> maximum colors + 1
                nextColor' = Collaboration.ColorId nextColor
            Collaboration.knownClients . at clId ?= Collaboration.Client currentTime nextColor'
            return nextColor'
