{-# LANGUAGE RecursiveDo #-}
module Main where

import           Common.Prelude
import           Control.Concurrent.Chan   (Chan)
import qualified Control.Concurrent.Chan   as Chan
import           Control.Concurrent.MVar
import           Data.DateTime             (getCurrentTime)
import qualified JS.Config                 as Config
import           JS.UUID                   (generateUUID)
import           JS.Visualizers            (mkVisualizersMap)
import           LunaStudio.Data.NodeValue (fromJSVisualizersMap)
import           NodeEditor.Event.Engine   (LoopRef (LoopRef))
import qualified NodeEditor.Event.Engine   as Engine
import qualified NodeEditor.React.Store    as Store
import qualified NodeEditor.React.View.App as App
import           NodeEditor.State.Global   (mkState)
import qualified React.Flux                as React
import           System.Random             (newStdGen)
import           WebSocket                 (WebSocket)


runApp :: Chan (IO ()) -> WebSocket -> IO ()
runApp chan socket = do
    random         <- newStdGen
    clientId       <- generateUUID
    initTime       <- getCurrentTime
    visualizersMap <- fromJSVisualizersMap <$> mkVisualizersMap
    let openedFile = Config.openedFile
    mdo
        let loop = LoopRef chan state
        Engine.scheduleInit loop
        appRef <- Store.createApp $ Engine.scheduleEvent loop
        React.reactRender Config.mountPoint (App.app appRef) ()
        let initState = mkState appRef clientId openedFile mempty visualizersMap initTime random
        state <- newMVar initState
        Engine.connectEventSources socket loop
    App.focus

main :: IO ()
main = do
    chan <- Chan.newChan
    Engine.withActiveConnection $ runApp chan
    Engine.start chan
