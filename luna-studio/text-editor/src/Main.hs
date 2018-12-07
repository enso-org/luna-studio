{-# LANGUAGE RecursiveDo #-}
module Main where

import           Common.Prelude
import           Common.ClientId         (clientId)
import           Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import           Control.Concurrent.MVar
import           System.Random           (newStdGen)

import           JS.Lexer                (installLexer)
import           JS.UUID                 (generateUUID)
import           TextEditor.Event.Engine (LoopRef (LoopRef))
import qualified TextEditor.Event.Engine as Engine
import           TextEditor.State.Global (mkState)
import           WebSocket               (WebSocket)

runApp :: Chan (IO ()) -> WebSocket -> IO ()
runApp chan socket = do
    random       <- newStdGen
    mdo
        let loop = LoopRef chan state
        Engine.scheduleInit loop
        let initState = mkState clientId random
        state <- newMVar initState
        Engine.connectEventSources socket loop

main :: IO ()
main = do
    cleanup <- installLexer
    chan <- Chan.newChan
    Engine.withActiveConnection $ runApp chan
    Engine.start chan
    cleanup
