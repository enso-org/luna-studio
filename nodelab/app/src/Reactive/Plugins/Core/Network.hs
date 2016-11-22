module Reactive.Plugins.Core.Network where

import           Utils.PreludePlus

import           Control.Concurrent.MVar

import           Control.Exception                                   (catch)
import           Data.DateTime                                       (getCurrentTime)
import           Data.Monoid                                         (Last (..))
import qualified Data.Text.Lazy                                      as Text
import           GHCJS.Prim                                          (JSException)

import           Reactive.Handlers                                   (AddHandler (..))
import qualified Reactive.Handlers                                   as Handlers

import qualified Event.Event                                         as Event
import qualified Event.Processors.Batch                              as BatchEventProcessor
import qualified Event.Processors.CustomEvent                        as CustomEventProcessor

import qualified Reactive.Plugins.Core.Action.Backend.Control        as Control
import qualified Reactive.Plugins.Core.Action.Backend.Graph          as Graph
import qualified Reactive.Plugins.Core.Action.Backend.ProjectManager as ProjectManager
import qualified Reactive.Plugins.Core.Action.Camera                 as Camera
import qualified Reactive.Plugins.Core.Action.Clipboard              as Clipboard
import qualified Reactive.Plugins.Core.Action.Collaboration          as Collaboration
import qualified Reactive.Plugins.Core.Action.Connect                as Connect
import qualified Reactive.Plugins.Core.Action.ConnectionPen          as ConnectionPen
import qualified Reactive.Plugins.Core.Action.Debug                  as Debug
import qualified Reactive.Plugins.Core.Action.Drag                   as Drag
import qualified Reactive.Plugins.Core.Action.General                as General
import qualified Reactive.Plugins.Core.Action.MultiSelection         as MultiSelection
import qualified Reactive.Plugins.Core.Action.Navigation             as Navigation
import qualified Reactive.Plugins.Core.Action.NodeSearcher           as NodeSearcher
import qualified Reactive.Plugins.Core.Action.Sandbox                as Sandbox
import qualified Reactive.Plugins.Core.Action.Tutorial               as Tutorial
import qualified Reactive.Plugins.Core.Action.Widget                 as Widget

import           Reactive.Commands.Command                           (Command, execCommand)
import           Reactive.State.Global                               (State)
import qualified Reactive.State.Global                               as Global

import qualified JS.Debug
import qualified JS.UI                                               as UI
import           JS.WebSocket                                        (WebSocket)

import qualified Data.JSString                                       as JSString


displayProcessingTime :: Bool
displayProcessingTime = False

foreign import javascript safe "console.time($1);"    consoleTimeStart' :: JSString.JSString -> IO ()
foreign import javascript safe "console.timeEnd($1);" consoleTimeEnd'   :: JSString.JSString -> IO ()


consoleTimeStart, consoleTimeEnd :: String -> IO ()
consoleTimeStart = consoleTimeStart' . JSString.pack
consoleTimeEnd   = consoleTimeEnd'   . JSString.pack

actions :: [Event.Event -> Maybe (Command State ())]
actions =  [ --Debug.toActionEv
        --    , Control.toAction
        --    , Widget.toAction
        --    , General.toAction
        --    , Camera.toAction
           Graph.toAction
        --    , MultiSelection.toAction
        --    , Drag.toAction
        --    , Connect.toAction
        --    , Navigation.toAction
        --    , Collaboration.toAction
        --    , NodeSearcher.toAction
        --    , ProjectManager.toAction
        --    , ConnectionPen.toAction
        --    , Tutorial.toAction
        --    , Sandbox.toAction
        --    , Clipboard.toAction
        --    , Debug.toAction
           ]

runCommands :: [Event.Event -> Maybe (Command State ())] -> Event.Event -> Command State ()
runCommands cmds event = sequence_ . catMaybes $ fmap ($ event) cmds

preprocessEvent :: Event.Event -> IO Event.Event
preprocessEvent ev = do
    let batchEvent = BatchEventProcessor.process  ev
    customEvent   <- CustomEventProcessor.process ev

    return $ fromMaybe ev $ getLast $ Last batchEvent <> Last customEvent

processEvent :: MVar State -> Event.Event -> IO ()
processEvent var ev = do
    state     <- takeMVar var
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
    flip catch (handleExcept realEvent) $ do
        newState <- execCommand (runCommands actions realEvent) state'
        UI.shouldRender
        when displayProcessingTime $
            consoleTimeEnd (realEvent ^. Event.name)
        putMVar var newState

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

handleExcept :: Event.Event -> JSException  -> IO ()
handleExcept event except =
    putStrLn $ "JavaScriptException: " <> show except <> "\n\nwhile processing: " <> show event
