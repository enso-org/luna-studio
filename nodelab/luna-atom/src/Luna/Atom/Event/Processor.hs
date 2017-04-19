module Luna.Atom.Event.Processor where

import           Control.Concurrent.MVar
import           Control.Exception                          (handle)
import           Data.DateTime                              (getCurrentTime)
import           Data.Monoid                                (Last (..))
import           GHCJS.Prim                                 (JSException)

import qualified JS.Debug
import           WebSocket                               (WebSocket)
import           Luna.Atom.Action.Command                 (Command, execCommand)
import           Luna.Atom.Event.Event                    (Event)
import qualified Luna.Atom.Event.Event                    as Event
import           Luna.Atom.Event.Loop                     (LoopRef)
import qualified Luna.Atom.Event.Loop                     as Loop
import qualified Luna.Atom.Event.Preprocessor.Batch       as BatchEventPreprocessor
import           Luna.Atom.Event.Source                   (AddHandler (..))
import qualified Luna.Atom.Event.Source                   as JSHandlers
import qualified Luna.Atom.Handler.Backend.ProjectManager as ProjectManager
import qualified Luna.Atom.Handler.Backend.Text           as Text
import           Luna.Prelude
import           Luna.Atom.State.Global                   (State)
import qualified Luna.Atom.State.Global                   as Global


displayProcessingTime :: Bool
displayProcessingTime = False

foreign import javascript safe "console.time($1);"    consoleTimeStart' :: JSString -> IO ()
foreign import javascript safe "console.timeEnd($1);" consoleTimeEnd'   :: JSString -> IO ()


consoleTimeStart, consoleTimeEnd :: String -> IO ()
consoleTimeStart = consoleTimeStart' . convert
consoleTimeEnd   = consoleTimeEnd'   . convert

actions :: LoopRef -> [Event -> Maybe (Command State ())]
actions loop =
    [ ProjectManager.handle
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
    when displayProcessingTime $ do
        consoleTimeStart $ (realEvent ^. Event.name) <>" show and force"
        --putStrLn . show . length $ show realEvent
        JS.Debug.error (convert $ realEvent ^. Event.name) realEvent
        consoleTimeEnd $ (realEvent ^. Event.name) <> " show and force"
        consoleTimeStart (realEvent ^. Event.name)
    timestamp <- getCurrentTime
    let state' = state & Global.lastEventTimestamp .~ timestamp
    handle (handleExcept state realEvent) $ do
        newState <- execCommand (runCommands (actions loop) realEvent ) state'
        when displayProcessingTime $
            consoleTimeEnd (realEvent ^. Event.name)
        return newState

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
