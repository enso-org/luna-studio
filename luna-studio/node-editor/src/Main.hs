{-# LANGUAGE RecursiveDo #-}
module Main where

import           Common.Prelude
import           Control.Concurrent.Chan              (Chan)
import qualified Control.Concurrent.Chan              as Chan
import           Control.Concurrent.MVar
import qualified Data.Map                             as Map
import qualified JS.Mount                             as Mount
import           JS.UUID                              (generateUUID)
import qualified JS.Visualizers                       as JS
import           LunaStudio.Data.NodeValue            (fromJSVisualizersMap)
import           NodeEditor.Event.Engine              (LoopRef (LoopRef))
import qualified NodeEditor.Event.Engine              as Engine
import qualified NodeEditor.React.Model.App           as App
import           NodeEditor.React.Model.Visualization (VisualizerId (VisualizerId), VisualizerType (InternalVisualizer))
import qualified NodeEditor.React.Store               as Store
import qualified NodeEditor.React.View.App            as App
import           NodeEditor.State.Global              (mkState)
import qualified React.Flux                           as React
import           System.Random                        (newStdGen)
import           WebSocket                            (WebSocket)


runApp :: Chan (IO ()) -> WebSocket -> IO ()
runApp chan socket = do
    random         <- newStdGen
    clientId       <- generateUUID
    JS.updateInternalVisualizers
    visualizersMap <- Map.mapKeys (flip VisualizerId InternalVisualizer) . fromJSVisualizersMap <$> JS.mkInternalVisualizersMap
    let openedFile = Mount.openedFile
    mdo
        let loop = LoopRef chan state
        Engine.scheduleInit loop
        appRef <- Store.createApp (App.mk openedFile) $ Engine.scheduleEvent loop
        React.reactRender Mount.mountPoint (App.app appRef) ()
        let initState = mkState appRef clientId mempty visualizersMap random
        state <- newMVar initState
        Engine.connectEventSources socket loop
    App.focus

main :: IO ()
main = do
    chan <- Chan.newChan
    Engine.withActiveConnection $ runApp chan
    Engine.start chan
