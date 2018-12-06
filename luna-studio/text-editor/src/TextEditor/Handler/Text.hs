module TextEditor.Handler.Text
    ( handle
    ) where

import           Common.Action.Command             (Command)
import           Common.Prelude
import qualified JS.Atom                           as JS
import qualified LunaStudio.API.Atom.Copy          as Copy
import qualified LunaStudio.API.Atom.GetBuffer     as GetBuffer
import qualified LunaStudio.API.Atom.Substitute    as Substitute
import qualified LunaStudio.API.Response           as Response
import           LunaStudio.Data.GraphLocation     (GraphLocation (GraphLocation))
import qualified TextEditor.Action.Batch           as ActBatch
import           TextEditor.Event.Batch            (BatchEvent (..))
import           TextEditor.Event.Event            (Event (Atom, Batch, Text))
import           TextEditor.Event.Internal         (InternalEvent (..))
import           TextEditor.Event.Text             (TextEvent (..))
import           TextEditor.Handler.Backend.Common (doNothing, doNothing2, handleResponse)
import           TextEditor.State.Global           (State)


handle :: Event -> Maybe (Command State ())
handle (Text (TextEvent location diffs)) = Just $ ActBatch.substitute location diffs
handle (Atom (GetBuffer filepath)) = Just $ ActBatch.getBuffer filepath
handle (Atom (FileChanged filepath)) = Just $ ActBatch.fileChanged filepath
handle (Atom (Copy filepath selections)) = Just $ ActBatch.copy filepath $ convert selections
handle (Atom (Paste selections content)) = Just $
    withJustM_ JS.activeLocation $ \location ->
        ActBatch.paste location (convert selections) content
handle (Atom Undo) = Just ActBatch.undo
handle (Atom Redo) = Just ActBatch.redo

handle (Batch (SubstituteResponse response)) = Just $ handleResponse response doNothing doNothing2
handle (Batch (BufferGetResponse  response)) = Just $ handleResponse response success doNothing2 where
    success result = do
        let uri  = response ^. Response.request . GetBuffer.filePath
            code = result ^. GetBuffer.code
        liftIO $ JS.setBuffer (convert uri) (convert code)
handle (Batch (CopyResponse  response)) = Just $ handleResponse response success doNothing2 where
    success result = do
        let uri  = response ^. Response.request . Copy.filePath
            code = result ^. Copy.code
        liftIO $ JS.setClipboard (convert uri) (convert code)

handle (Batch (SubstituteUpdate (Substitute.Update path diffs))) =
    Just $ liftIO $ JS.insertCode $ TextEvent (GraphLocation path def) diffs


handle _ = Nothing
