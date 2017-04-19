module Node.Editor.Event.Processor where

import           Control.Concurrent.MVar
import           Control.Exception                          (handle)
import           Data.DateTime                              (getCurrentTime)
import           Data.Monoid                                (Last (..))
import           GHCJS.Prim                                 (JSException)

import qualified JS.Debug
import           WebSocket                                  (WebSocket)
import           Node.Editor.Action.Command                 (Command, execCommand)
import           Node.Editor.Action.State.App               (renderIfNeeded)
import           Node.Editor.Event.Event                    (Event)
import qualified Node.Editor.Event.Event                    as Event
import           Node.Editor.Event.Loop                     (LoopRef)
import qualified Node.Editor.Event.Loop                     as Loop
import qualified Node.Editor.Event.Preprocessor.Batch       as BatchEventPreprocessor
import qualified Node.Editor.Event.Preprocessor.CustomEvent as CustomEventPreprocessor
import qualified Node.Editor.Event.Preprocessor.Shortcut    as ShortcutEventPreprocessor
import           Node.Editor.Event.Source                   (AddHandler (..))
import qualified Node.Editor.Event.Source                   as JSHandlers
import qualified Node.Editor.Handler.App                    as App
import qualified Node.Editor.Handler.Backend.Control        as Control
import qualified Node.Editor.Handler.Backend.Graph          as Graph
import qualified Node.Editor.Handler.Breadcrumbs            as Breadcrumbs
import qualified Node.Editor.Handler.Camera                 as Camera
import qualified Node.Editor.Handler.Clipboard              as Clipboard
import qualified Node.Editor.Handler.CodeEditor             as CodeEditor
import qualified Node.Editor.Handler.Collaboration          as Collaboration
import qualified Node.Editor.Handler.Connect                as Connect
import qualified Node.Editor.Handler.ConnectionPen          as ConnectionPen
import qualified Node.Editor.Handler.Debug                  as Debug
import qualified Node.Editor.Handler.MultiSelection         as MultiSelection
import qualified Node.Editor.Handler.Navigation             as Navigation
import qualified Node.Editor.Handler.Node                   as Node
import qualified Node.Editor.Handler.Port                   as Port
import qualified Node.Editor.Handler.Searcher               as Searcher
import qualified Node.Editor.Handler.Sidebar                as Sidebar
import qualified Node.Editor.Handler.Undo                   as Undo
import qualified Node.Editor.Handler.Visualization          as Visualization
import           Luna.Prelude
import           Luna.Report
import           Node.Editor.State.Global                   (State)
import qualified Node.Editor.State.Global                   as Global


displayProcessingTime :: Bool
displayProcessingTime = False

foreign import javascript safe "console.time($1);"    consoleTimeStart' :: JSString -> IO ()
foreign import javascript safe "console.timeEnd($1);" consoleTimeEnd'   :: JSString -> IO ()


consoleTimeStart, consoleTimeEnd :: String -> IO ()
consoleTimeStart = consoleTimeStart' . convert
consoleTimeEnd   = consoleTimeEnd'   . convert

actions :: LoopRef -> [Event -> Maybe (Command State ())]
actions loop =
    [ App.handle
    , Breadcrumbs.handle
    , Camera.handle
    , Clipboard.handle
    , CodeEditor.handle
    , Collaboration.handle
    , Connect.handle
    , ConnectionPen.handle
    , Control.handle
    , Debug.handle
    , Debug.handleEv
    , Graph.handle
    , MultiSelection.handle
    , Navigation.handle
    , Node.handle
    , Port.handle
    , Sidebar.handle
    , Undo.handle
    , Searcher.handle (scheduleEvent loop)
    , Visualization.handle
    ]

runCommands :: [Event -> Maybe (Command State ())] -> Event -> Command State ()
runCommands cmds event = sequence_ . catMaybes $ fmap ($ event) cmds

preprocessEvent :: Event -> IO Event
preprocessEvent ev = do
    let batchEvent    = BatchEventPreprocessor.process ev
        shortcutEvent = ShortcutEventPreprocessor.process ev
    customEvent   <- CustomEventPreprocessor.process ev
    return $ fromMaybe ev $ getLast $ Last batchEvent <> Last customEvent <> Last shortcutEvent

processEvent :: LoopRef -> Event -> IO ()
processEvent loop ev = modifyMVar_ (loop ^. Loop.state) $ \state -> do
    realEvent <- preprocessEvent ev
    when displayProcessingTime $ do
        consoleTimeStart $ (realEvent ^. Event.name) <>" show and force"
        --putStrLn . show . length $ show realEvent
        JS.Debug.error (convert $ realEvent ^. Event.name) realEvent
        consoleTimeEnd $ (realEvent ^. Event.name) <> " show and force"
        consoleTimeStart (realEvent ^. Event.name)
    timestamp <- getCurrentTime
    let state' = state & Global.lastEventTimestamp .~ timestamp
    handle (handleExcept state realEvent) $ do
        newState <- execCommand (runCommands (actions loop) realEvent >> renderIfNeeded) state'
        when displayProcessingTime $
            consoleTimeEnd (realEvent ^. Event.name)
        return newState

connectEventSources :: WebSocket -> LoopRef -> IO ()
connectEventSources conn loop = do
    let handlers = [ JSHandlers.webSocketHandler conn
                   , JSHandlers.atomHandler
                   , JSHandlers.sceneResizeHandler
                   ]
        mkSource (AddHandler rh) = rh $ scheduleEvent loop
    sequence_ $ mkSource <$> handlers

handleExcept :: State -> Event -> JSException -> IO State
handleExcept oldState event except = do
    error $ "JavaScriptException: " <> show except <> "\n\nwhile processing: " <> show event
    return oldState


scheduleEvent :: LoopRef -> Event -> IO ()
scheduleEvent loop = Loop.schedule loop . processEvent loop

scheduleInit :: LoopRef -> IO ()
scheduleInit loop = scheduleEvent loop Event.Init
