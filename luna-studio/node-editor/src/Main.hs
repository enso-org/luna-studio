{-# LANGUAGE RecursiveDo #-}
module Main where

import           Common.Prelude
import           Control.Concurrent.Chan    (Chan)
import qualified Control.Concurrent.Chan    as Chan
import           Control.Concurrent.MVar
import qualified JS.Mount                   as Mount
import           JS.UUID                    (generateUUID)
import           NodeEditor.Event.Engine    (LoopRef (LoopRef))
import qualified NodeEditor.Event.Engine    as Engine
import qualified NodeEditor.React.Model.App as App
import qualified NodeEditor.React.Store     as Store
import qualified NodeEditor.React.View.App  as App
import           NodeEditor.State.Global    (mkState)
import           System.Random              (newStdGen)
import           WebSocket                  (WebSocket)


runApp :: Chan (IO ()) -> WebSocket -> IO ()
runApp chan socket = do
    random         <- newStdGen
    clientId       <- generateUUID
    let openedFile = Mount.openedFile
    mdo
        let loop = LoopRef chan state
        Engine.scheduleInit loop
        appRef <- Store.createApp (App.mk openedFile) $ Engine.scheduleEvent loop
        let initState = mkState appRef clientId random
        state <- newMVar initState
        Engine.connectEventSources socket loop
    pure ()

main :: IO ()
main = do
    chan <- Chan.newChan
    Engine.withActiveConnection $ runApp chan
    Engine.start chan
