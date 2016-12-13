{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Collaboration
    ( updateClient
    ) where

import qualified Data.Map.Lazy                as Map
import           Luna.Studio.Prelude

import           Empire.API.Graph.Collaboration (ClientId)
import           Reactive.Commands.Command    (Command)
import           Reactive.State.Collaboration (ColorId)
import qualified Reactive.State.Collaboration as Collaboration
import           Reactive.State.Global        (State)
import qualified Reactive.State.Global        as Global

updateClient :: ClientId -> Command State ColorId
updateClient clId = do
    mayCurrentData <- preuse $ Global.collaboration . Collaboration.knownClients . ix clId
    currentTime  <- use Global.lastEventTimestamp
    zoom Global.collaboration $ case mayCurrentData of
        Just currentData -> do
            Collaboration.knownClients . ix clId . Collaboration.lastSeen .= currentTime
            return $ currentData ^. Collaboration.colorId
        Nothing          -> do
            clients <- use $ Collaboration.knownClients
            let colors = Collaboration.unColorId <$> (view Collaboration.colorId) <$> Map.elems clients
                nextColor = case colors of
                    [] -> 0
                    _  -> (maximum colors) + 1
                nextColor' = Collaboration.ColorId nextColor
            Collaboration.knownClients . at clId ?= (Collaboration.Client currentTime nextColor')
            return nextColor'
