module NodeEditor.Event.Processor where

import           Control.Concurrent.MVar
import           Control.Exception                      (handle)
import           Data.DateTime                          (getCurrentTime)
import           Data.Monoid                            (Last (..))
import           GHCJS.Prim                             (JSException)

import           Common.Prelude
import           Common.Report
import qualified JS.Debug
import           NodeEditor.Action.Command              (Command, execCommand)
import           NodeEditor.Action.State.App            (renderIfNeeded)
import           NodeEditor.Event.Event                 (Event)
import qualified NodeEditor.Event.Event                 as Event
import           NodeEditor.Event.Loop                  (LoopRef)
import qualified NodeEditor.Event.Loop                  as Loop
import qualified NodeEditor.Event.Preprocessor.Batch    as BatchEventPreprocessor
import qualified NodeEditor.Event.Preprocessor.Shortcut as ShortcutEventPreprocessor
import           NodeEditor.Event.Source                (AddHandler (..))
import qualified NodeEditor.Event.Source                as JSHandlers
import qualified NodeEditor.Handler.App                 as App
import qualified NodeEditor.Handler.Backend.Control     as Control
import qualified NodeEditor.Handler.Backend.Graph       as Graph
import qualified NodeEditor.Handler.Breadcrumbs         as Breadcrumbs
import qualified NodeEditor.Handler.Camera              as Camera
import qualified NodeEditor.Handler.Clipboard           as Clipboard
import qualified NodeEditor.Handler.Connect             as Connect
import qualified NodeEditor.Handler.ConnectionPen       as ConnectionPen
import qualified NodeEditor.Handler.MockMonads          as MockMonads
import qualified NodeEditor.Handler.MultiSelection      as MultiSelection
import qualified NodeEditor.Handler.Navigation          as Navigation
import qualified NodeEditor.Handler.Node                as Node
import qualified NodeEditor.Handler.Port                as Port
import qualified NodeEditor.Handler.Searcher            as Searcher
import qualified NodeEditor.Handler.Sidebar             as Sidebar
import qualified NodeEditor.Handler.Undo                as Undo
import qualified NodeEditor.Handler.Visualization       as Visualization
import           NodeEditor.State.Global                (State)
import qualified NodeEditor.State.Global                as Global
import           WebSocket                              (WebSocket)


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
    , Connect.handle
    , ConnectionPen.handle
    , Control.handle
    , Graph.handle
    , MultiSelection.handle
    , Navigation.handle
    , Node.handle
    , Port.handle
    , Sidebar.handle
    , Undo.handle
    , Searcher.handle (scheduleEvent loop)
    , Visualization.handle
    , MockMonads.handle
    ]

runCommands :: [Event -> Maybe (Command State ())] -> Event -> Command State ()
runCommands cmds event = sequence_ . catMaybes $ fmap ($ event) cmds

preprocessEvent :: Event -> IO Event
preprocessEvent ev = do
    let batchEvent    = BatchEventPreprocessor.process ev
        shortcutEvent = ShortcutEventPreprocessor.process ev
    return $ fromMaybe ev $ getLast $ Last batchEvent <> Last shortcutEvent

processEvent :: LoopRef -> Event -> IO ()
processEvent loop ev = modifyMVar_ (loop ^. Loop.state) $ \state -> do
    realEvent <- preprocessEvent ev
    when displayProcessingTime $ do
        consoleTimeStart $ (realEvent ^. Event.name) <>" show and force"
        JS.Debug.error (convert $ realEvent ^. Event.name) ()
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
                   , JSHandlers.movementHandler
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
