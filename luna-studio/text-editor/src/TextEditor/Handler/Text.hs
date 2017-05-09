module TextEditor.Handler.Text
    ( handle
    ) where

import           JS.Atom
import           Common.Prelude
import qualified Empire.API.Atom.GetBuffer  as GetBuffer
import qualified Empire.API.Atom.Substitute as Substitute
import qualified Empire.API.Response                    as Response


import           TextEditor.Action.Command         (Command)
import           TextEditor.State.Global           (State)
import           TextEditor.Event.Text             (TextEvent(..))
import           TextEditor.Event.Internal         (InternalEvent(..), ActionType(..))

import           TextEditor.Event.Batch                (Event (..))
import qualified TextEditor.Event.Event                as Event
import qualified TextEditor.Action.Batch               as ActBatch
import           TextEditor.Handler.Backend.Common     (doNothing, handleResponse)


handle :: Event.Event -> Maybe (Command State ())
handle (Event.Text (TextEvent filepath start stop text cursor)) = Just $ putStrLn "textEvent"  >>  ActBatch.substitute filepath start stop text cursor >> putStrLn "textEvent after send req"
handle (Event.Atom (InternalEvent GetBuffer filepath Nothing)) = Just $ putStrLn "getBuffer" >> ActBatch.getBuffer filepath Nothing
handle (Event.Atom (InternalEvent Copy filepath selections)) = Just $ putStrLn "getBuffer" >> ActBatch.getBuffer filepath selections

handle (Event.Batch (SubstituteResponse response)) = Just $ handleResponse response doNothing doNothing
handle (Event.Batch (BufferGetResponse  response)) = Just $ handleResponse response success doNothing where
   success result = do
       let uri  = response ^. Response.request . GetBuffer.filePath
           code = result ^. GetBuffer.code
       liftIO $ pushBuffer (convert uri) (convert code)


handle (Event.Batch (SubstituteUpdate (Substitute.Update path start end text cursor))) = Just $ liftIO $ pushCode $ TextEvent path start end text cursor


handle _ = Nothing
