module TextEditor.Event.Processor where

import           Control.Concurrent.MVar
import           Control.Exception                   (handle)
import           Data.Monoid                         (Last (..))
import           GHCJS.Prim                          (JSException)

import           Common.Action.Command               (Command, execCommand)
import qualified Common.Analytics                    as Analytics
import           Common.Prelude
import           TextEditor.Event.Event              (Event)
import qualified TextEditor.Event.Event              as Event
import           TextEditor.Event.Loop               (LoopRef)
import qualified TextEditor.Event.Loop               as Loop
import qualified TextEditor.Event.Preprocessor.Batch as BatchEventPreprocessor
import           TextEditor.Event.Source             (AddHandler (..))
import qualified TextEditor.Event.Source             as JSHandlers
import qualified TextEditor.Handler.Control          as Control
import qualified TextEditor.Handler.ProjectManager   as ProjectManager
import qualified TextEditor.Handler.Text             as Text
import           TextEditor.State.Global             (State)
import           WebSocket                           (WebSocket)


actions :: LoopRef -> [Event -> Maybe (Command State ())]
actions _ =
    [ Control.handle
    , ProjectManager.handle
    , Text.handle
    ]

runCommands :: [Event -> Maybe (Command State ())] -> Event -> Command State ()
runCommands cmds event = sequence_ . catMaybes $ fmap ($ event) cmds

preprocessEvent :: Event -> IO Event
preprocessEvent ev = do
    let batchEvent    = BatchEventPreprocessor.process ev
    return $ fromMaybe ev $ getLast $ Last batchEvent

processEvent :: LoopRef -> Event -> IO ()
processEvent loop ev = modifyMVar_ (loop ^. Loop.state) $ \state -> do
    realEvent <- preprocessEvent ev
    Analytics.track realEvent
    handle (handleExcept state realEvent) $
        execCommand (runCommands (actions loop) realEvent ) state

connectEventSources :: WebSocket -> LoopRef -> IO ()
connectEventSources conn loop = do
    let handlers = [ JSHandlers.webSocketHandler conn
                   , JSHandlers.textHandler
                   , JSHandlers.fileHandler
                   ]
        mkSource (AddHandler rh) = rh $ scheduleEvent loop
    sequence_ $ mkSource <$> handlers

handleExcept :: State -> Event -> JSException -> IO State
handleExcept oldState event except = do
    putStrLn $ "JavaScriptException: " <> show except <> "\n\nwhile processing: " <> show event
    return oldState


scheduleEvent :: LoopRef -> Event -> IO ()
scheduleEvent loop = Loop.schedule loop . processEvent loop

scheduleInit :: LoopRef -> IO ()
scheduleInit loop = scheduleEvent loop Event.Init
