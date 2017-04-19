module Luna.Atom.Handler.Backend.Text
    ( handle
    ) where

import           JS.Atom
import           Luna.Prelude
import qualified Empire.API.Atom.GetBuffer  as GetBuffer
import           Empire.API.Atom.GetBuffer  (Result(..))
import qualified Empire.API.Atom.Substitute as Substitute
import qualified Empire.API.Response                    as Response


import           Luna.Atom.Action.Command         (Command)
import           Luna.Atom.State.Global           (State)
import           Luna.Atom.Event.Text             (TextEvent(..))
import           Luna.Atom.Event.Internal         (InternalEvent(..), ActionType(..))

import           Luna.Atom.Event.Batch                (Event (..))
import qualified Luna.Atom.Event.Event                as Event
import qualified Luna.Atom.Action.Batch               as ActBatch
import           Luna.Atom.Handler.Backend.Common     (doNothing, handleResponse)


handle :: Event.Event -> Maybe (Command State ())
handle (Event.Text (TextEvent filepath start stop text cursor)) = Just $ putStrLn "textEvent"  >>  ActBatch.substitute filepath start stop text cursor >> putStrLn "textEvent after send req"
handle (Event.Atom (InternalEvent GetBuffer filepath)) = Just $ putStrLn "getBuffer" >> ActBatch.getBuffer filepath Nothing

handle (Event.Batch (SubstituteResponse response)) = Just $ handleResponse response doNothing doNothing
handle (Event.Batch (BufferGetResponse  response)) = Just $ handleResponse response success doNothing where
   success result = do
       let uri  = response ^. Response.request . GetBuffer.filePath
           code = result ^. GetBuffer.code
       liftIO $ pushBuffer (convert uri) (convert code)


handle (Event.Batch (SubstituteUpdate (Substitute.Update path start end text cursor))) = Just $ liftIO $ pushCode $ TextEvent path start end text cursor


handle _ = Nothing
