module Reactive.Plugins.Core.Network where

import           Luna.Studio.Prelude

import           Control.Concurrent.MVar

import           Control.Exception                          (catch)
import           Data.DateTime                              (getCurrentTime)
import           Data.Monoid                                (Last (..))
import qualified Data.Text                                  as Text
import           GHCJS.Prim                                 (JSException)

import           Reactive.Handlers                          (AddHandler (..))
import qualified Reactive.Handlers                          as Handlers

import           Event.Event                                (Event)
import qualified Event.Event                                as Event
import qualified Event.Processors.Batch                     as BatchEventProcessor
import qualified Event.Processors.CustomEvent               as CustomEventProcessor

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
import qualified Luna.Studio.Handler.Drag                   as Drag
import qualified Luna.Studio.Handler.MultiSelection         as MultiSelection
import qualified Luna.Studio.Handler.Navigation             as Navigation
import qualified Luna.Studio.Handler.Node                   as Node
import qualified Luna.Studio.Handler.Searcher               as Searcher
import qualified Luna.Studio.Handler.Tutorial               as Tutorial
import qualified Luna.Studio.Handler.Widget                 as Widget

import           Luna.Studio.Action.Command                 (Command, execCommand)
import           Luna.Studio.State.Global                   (State)
import qualified Luna.Studio.State.Global                   as Global

import qualified JS.Debug
import           JS.WebSocket                               (WebSocket)

import qualified Data.JSString                              as JSString


displayProcessingTime :: Bool
displayProcessingTime = False

foreign import javascript safe "console.time($1);"    consoleTimeStart' :: JSString.JSString -> IO ()
foreign import javascript safe "console.timeEnd($1);" consoleTimeEnd'   :: JSString.JSString -> IO ()


consoleTimeStart, consoleTimeEnd :: String -> IO ()
consoleTimeStart = consoleTimeStart' . JSString.pack
consoleTimeEnd   = consoleTimeEnd'   . JSString.pack

actions :: [Event -> Maybe (Command State ())]
actions =  [ App.toAction
           , Breadcrumbs.toAction
           , CodeEditor.toAction
           , Connect.toAction
           , Drag.toAction
           , Graph.toAction
           , MultiSelection.toAction
           , Navigation.toAction
           , Node.toAction
           , ProjectManager.toAction
           , Searcher.toAction
           , Camera.toAction
        --Debug.toActionEv
        --    , Control.toAction
        --    , Widget.toAction
        --    , Collaboration.toAction
           , ConnectionPen.toAction
        --    , Tutorial.toAction
        --    , Clipboard.toAction
        --    , Debug.toAction
           ]

runCommands :: [Event -> Maybe (Command State ())] -> Event -> Command State ()
runCommands cmds event = sequence_ . catMaybes $ fmap ($ event) cmds

preprocessEvent :: Event -> IO Event
preprocessEvent ev = do
    let batchEvent = BatchEventProcessor.process  ev
    customEvent   <- CustomEventProcessor.process ev

    return $ fromMaybe ev $ getLast $ Last batchEvent <> Last customEvent

processEvent :: MVar State -> Event -> IO ()
processEvent var ev = modifyMVar_ var $ \state -> do
    realEvent <- preprocessEvent ev
    when displayProcessingTime $ do
        consoleTimeStart $ (realEvent ^. Event.name) <>" show and force"
        --putStrLn . show . length $ show realEvent
        JS.Debug.error (Text.pack $ realEvent ^. Event.name) realEvent
        consoleTimeEnd $ (realEvent ^. Event.name) <> " show and force"
        consoleTimeStart (realEvent ^. Event.name)
    jsState   <- Handlers.getJSState
    timestamp <- getCurrentTime
    let state' = state & Global.jsState .~ jsState
                       & Global.lastEventTimestamp .~ timestamp
    flip catch (handleExcept state realEvent) $ do
        newState <- execCommand (runCommands actions realEvent) state'
        when displayProcessingTime $
            consoleTimeEnd (realEvent ^. Event.name)
        return newState

makeNetworkDescription :: WebSocket -> MVar State -> IO ()
makeNetworkDescription conn state = do
    let handlers = [ --Handlers.resizeHandler
                --    , Handlers.mouseDownHandler
                --    , Handlers.mouseUpHandler
                --    , Handlers.mouseMovedHandler
                --    , Handlers.mouseDblClickHandler
                --    , Handlers.mouseWheelHandler
                --    , Handlers.keyDownHandler
                --    , Handlers.keyPressedHandler
                --    , Handlers.keyUpHandler
                    Handlers.webSocketHandler conn
                --    , Handlers.connectionPenHandler
                --    , Handlers.textEditorHandler
                --    , Handlers.customEventHandler
                --    , Handlers.copyClipboardHandler
                --    , Handlers.cutClipboardHandler
                --    , Handlers.pasteClipboardHandler
                   ]

    let registerHandler (AddHandler rh) = rh (processEvent state)

    sequence_ $ registerHandler <$> handlers

handleExcept :: State -> Event -> JSException -> IO State
handleExcept oldState event except = do
    putStrLn $ "JavaScriptException: " <> show except <> "\n\nwhile processing: " <> show event
    return oldState
