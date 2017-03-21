module Luna.Studio.Handler.Backend.Text
    ( handle
    ) where

import           JS.Atom
import           Luna.Studio.Prelude
import qualified Empire.API.Atom.GetBuffer  as GetBuffer
import           Empire.API.Atom.GetBuffer  (Result(..))
import qualified Empire.API.Atom.Substitute as Substitute
import qualified Empire.API.Graph.GetProgram            as GetProgram
import qualified Empire.API.Response                    as Response


import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.State.Global           (State)
import           Luna.Studio.Event.Text             (TextEvent(..))
import           Luna.Studio.Event.Internal         (InternalEvent(..), ActionType(..))

import           Luna.Studio.Event.Batch                (Event (..))
import qualified Luna.Studio.Event.Event                as Event
import qualified Luna.Studio.Action.Batch               as ActBatch
import           Luna.Studio.Handler.Backend.Common     (doNothing, handleResponse)


handle :: Event.Event -> Maybe (Command State ())
handle (Event.Text (TextEvent filepath start stop text cursor)) = Just $ ActBatch.substitute filepath start stop text cursor
handle (Event.Atom (InternalEvent GetBuffer filepath)) = Just $ ActBatch.getBuffer filepath Nothing

handle (Event.Batch (SubstituteResponse response)) = Just $ handleResponse response doNothing
handle (Event.Batch (BufferGetResponse  response)) = Just $ handleResponse response $ \_ result -> do
    let uri  = response ^. Response.request . GetBuffer.filePath
        code = result ^. GetBuffer.code
    liftIO $ pushBuffer (convert uri) (convert code)


handle (Event.Batch (SubstituteUpdate (Substitute.Update path start end text cursor))) = Just $ liftIO $ pushCode $ TextEvent path start end text cursor


handle _ = Nothing
