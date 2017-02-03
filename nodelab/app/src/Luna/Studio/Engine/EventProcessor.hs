module Luna.Studio.Engine.EventProcessor where

import           Control.Concurrent.Chan                    (Chan)
import qualified Control.Concurrent.Chan                    as Chan
import           Control.Concurrent.MVar
import           Control.Exception                          (catch)
import           Control.Monad                              (forever)
import           Data.DateTime                              (getCurrentTime)
import           Data.Monoid                                (Last (..))
import           GHCJS.Prim                                 (JSException)

import qualified JS.Debug
import           JS.WebSocket                               (WebSocket)
import           Luna.Studio.Action.Command                 (Command, execCommand)
import           Luna.Studio.Engine.JSHandlers              (AddHandler (..))
import qualified Luna.Studio.Engine.JSHandlers              as JSHandlers
import           Luna.Studio.Event.Event                    (Event)
import qualified Luna.Studio.Event.Event                    as Event
import qualified Luna.Studio.Event.Preprocessor.Batch       as BatchEventPreprocessor
import qualified Luna.Studio.Event.Preprocessor.CustomEvent as CustomEventPreprocessor
import qualified Luna.Studio.Event.Preprocessor.Shortcut    as ShortcutEventPreprocessor
import qualified Luna.Studio.Handler.App                    as App
import qualified Luna.Studio.Handler.Backend.Control        as Control
import qualified Luna.Studio.Handler.Backend.Graph          as Graph
import qualified Luna.Studio.Handler.Backend.ProjectManager as ProjectManager
import qualified Luna.Studio.Handler.Breadcrumbs            as Breadcrumbs
import qualified Luna.Studio.Handler.Camera                 as Camera
import qualified Luna.Studio.Handler.Clipboard              as Clipboard
import qualified Luna.Studio.Handler.CodeEditor             as CodeEditor
import qualified Luna.Studio.Handler.Collaboration          as Collaboration
import qualified Luna.Studio.Handler.Connect                as Connect
import qualified Luna.Studio.Handler.ConnectionPen          as ConnectionPen
import qualified Luna.Studio.Handler.Debug                  as Debug
import qualified Luna.Studio.Handler.MultiSelection         as MultiSelection
import qualified Luna.Studio.Handler.Navigation             as Navigation
import qualified Luna.Studio.Handler.Node                   as Node
import qualified Luna.Studio.Handler.Searcher               as Searcher
import qualified Luna.Studio.Handler.Visualization          as Visualization
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global                   (State)
import qualified Luna.Studio.State.Global                   as Global



displayProcessingTime :: Bool
displayProcessingTime = False

foreign import javascript safe "console.time($1);"    consoleTimeStart' :: JSString -> IO ()
foreign import javascript safe "console.timeEnd($1);" consoleTimeEnd'   :: JSString -> IO ()


consoleTimeStart, consoleTimeEnd :: String -> IO ()
consoleTimeStart = consoleTimeStart' . convert
consoleTimeEnd   = consoleTimeEnd'   . convert

actions :: [Event -> Maybe (Command State ())]
actions =  [ App.handle
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
           , ProjectManager.handle
           , Searcher.handle
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

processEvent :: MVar State -> Event -> IO ()
processEvent var ev = modifyMVar_ var $ \state -> do
    realEvent <- preprocessEvent ev
    when displayProcessingTime $ do
        consoleTimeStart $ (realEvent ^. Event.name) <>" show and force"
        --putStrLn . show . length $ show realEvent
        JS.Debug.error (convert $ realEvent ^. Event.name) realEvent
        consoleTimeEnd $ (realEvent ^. Event.name) <> " show and force"
        consoleTimeStart (realEvent ^. Event.name)
    timestamp <- getCurrentTime
    let state' = state & Global.lastEventTimestamp .~ timestamp
    flip catch (handleExcept state realEvent) $ do
        newState <- execCommand (runCommands actions realEvent >> Global.renderIfNeeded) state'
        when displayProcessingTime $
            consoleTimeEnd (realEvent ^. Event.name)
        return newState

connectEventSources :: WebSocket ->  Chan (IO ()) -> MVar State -> IO ()
connectEventSources conn chan state = do
    let handlers = [ JSHandlers.webSocketHandler conn
                   , JSHandlers.atomHandler
                   , JSHandlers.sceneResizeHandler
                   ]
        mkSource (AddHandler rh) = rh $ scheduleEvent chan state
    sequence_ $ mkSource <$> handlers

handleExcept :: State -> Event -> JSException -> IO State
handleExcept oldState event except = do
    putStrLn $ "JavaScriptException: " <> show except <> "\n\nwhile processing: " <> show event
    return oldState

scheduleEvent :: Chan (IO ()) -> MVar State -> Event -> IO ()
scheduleEvent chan = Chan.writeChan chan .: processEvent

startLoop :: Chan (IO ()) -> IO ()
startLoop = forever . join . Chan.readChan
