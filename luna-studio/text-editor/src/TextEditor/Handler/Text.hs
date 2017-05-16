module TextEditor.Handler.Text
    ( handle
    ) where

import           Common.Prelude
import qualified LunaStudio.API.Atom.GetBuffer         as GetBuffer
import qualified LunaStudio.API.Atom.Substitute        as Substitute
import qualified LunaStudio.API.Response               as Response
import           JS.Atom


import           TextEditor.Action.Command         (Command)
import           TextEditor.Event.Internal         (ActionType (..), InternalEvent (..))
import           TextEditor.Event.Text             (TextEvent (..))
import           TextEditor.State.Global           (State)

import qualified TextEditor.Action.Batch           as ActBatch
import           TextEditor.Event.Batch            (Event (..))
import qualified TextEditor.Event.Event            as Event
import           TextEditor.Handler.Backend.Common (doNothing, handleResponse)


handle :: Event.Event -> Maybe (Command State ())
handle (Event.Text (TextEvent filepath start stop text cursor _)) = Just $ putStrLn "textEvent"  >>  ActBatch.substitute filepath start stop text cursor >> putStrLn "textEvent after send req"
handle (Event.Atom (InternalEvent GetBuffer filepath Nothing)) = Just $ putStrLn "getBuffer" >> ActBatch.getBuffer filepath Nothing
handle (Event.Atom (InternalEvent Copy filepath selections)) = Just $ putStrLn "getBuffer" >> ActBatch.getBuffer filepath selections

handle (Event.Batch (SubstituteResponse response)) = Just $ handleResponse response doNothing doNothing
handle (Event.Batch (BufferGetResponse  response)) = Just $ handleResponse response success doNothing where
    success (GetBuffer.Result code tags) = do
        let uri  = response ^. Response.request . GetBuffer.filePath
        liftIO $ pushBuffer (convert uri) (convert code) (convertTags tags)


handle (Event.Batch (SubstituteUpdate (Substitute.Update path start end text cursor tags))) =
    Just $ pushCode $ TextEvent path start end text cursor tags


handle _ = Nothing
