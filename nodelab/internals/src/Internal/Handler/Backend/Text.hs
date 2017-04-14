module Internal.Handler.Backend.Text
    ( handle
    ) where

import           JS.Atom
import           Internal.Prelude
import qualified Empire.API.Atom.GetBuffer  as GetBuffer
import           Empire.API.Atom.GetBuffer  (Result(..))
import qualified Empire.API.Atom.Substitute as Substitute
import qualified Empire.API.Response                    as Response


import           Internal.Action.Command         (Command)
import           Internal.State.Global           (State)
import           Internal.Event.Text             (TextEvent(..))
import           Internal.Event.Internal         (InternalEvent(..), ActionType(..))

import           Internal.Event.Batch                (Event (..))
import qualified Internal.Event.Event                as Event
import qualified Internal.Action.Batch               as ActBatch
import           Internal.Handler.Backend.Common     (doNothing, handleResponse)


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
