module TextEditor.Handler.Text
    ( handle
    ) where

import           Common.Action.Command             (Command)
import           Common.Prelude
import           JS.Atom
import qualified LunaStudio.API.Atom.GetBuffer     as GetBuffer
import qualified LunaStudio.API.Atom.Substitute    as Substitute
import qualified LunaStudio.API.Response           as Response
import           LunaStudio.Data.GraphLocation     (GraphLocation (GraphLocation))
import qualified TextEditor.Action.Batch           as ActBatch
import           TextEditor.Event.Batch            (Event (..))
import qualified TextEditor.Event.Event            as Event
import           TextEditor.Event.Internal         (InternalEvent (..))
import           TextEditor.Event.Text             (TextEvent (..))
import           TextEditor.Handler.Backend.Common (doNothing, handleResponse)
import           TextEditor.State.Global           (State)


handle :: Event.Event -> Maybe (Command State ())
handle (Event.Text (TextEvent location start end text cursor)) = Just $ ActBatch.substitute location start end text cursor
handle (Event.Atom (GetBuffer filepath)) = Just $ ActBatch.getBuffer filepath Nothing
handle (Event.Atom (FileChanged filepath)) = Just $ ActBatch.fileChanged filepath
handle (Event.Atom (Copy filepath selections)) = Just $ ActBatch.getBuffer filepath selections

handle (Event.Batch (SubstituteResponse response)) = Just $ handleResponse response doNothing doNothing
handle (Event.Batch (BufferGetResponse  response)) = Just $ timeIt "BufferGetResponse" $ handleResponse response success doNothing where
   success result = do
       let uri  = response ^. Response.request . GetBuffer.filePath
           code = result ^. GetBuffer.code
       liftIO (pushBuffer (convert uri) (convert code)) <!!> "pushBuffer"


handle (Event.Batch (SubstituteUpdate (Substitute.Update path start end text cursor))) =
    Just $ liftIO $ pushCode $ TextEvent (GraphLocation path def) start end text cursor


handle _ = Nothing
