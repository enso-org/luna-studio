module TextEditor.Handler.ProjectManager
    ( handle
    ) where

import           Common.Action.Command             (Command)
import           Common.Prelude
import           Data.Char                         (toUpper)
import           JS.Atom
import qualified LunaStudio.API.Atom.IsSaved       as IsSaved
import qualified LunaStudio.API.Atom.OpenFile      as OpenFile
import qualified LunaStudio.API.Response           as Response
import qualified TextEditor.Action.Batch           as BatchCmd (closeFile, isSaved, openFile, saveFile, setProject)
import qualified TextEditor.Event.Batch            as Batch
import           TextEditor.Event.Event            (Event (Atom, Batch))
import           TextEditor.Event.Internal         (InternalEvent (..))
import           TextEditor.Handler.Backend.Common (doNothing, handleResponse)
import           TextEditor.State.Global           (State)


handle :: Event -> Maybe (Command State ())
handle (Atom (CloseFile  path)) = Just $ BatchCmd.closeFile  path
handle (Atom (IsSaved    path)) = Just $ BatchCmd.isSaved    path
handle (Atom (OpenFile   path)) = Just $ BatchCmd.openFile   path
handle (Atom (SaveFile   path)) = Just $ BatchCmd.saveFile   path
handle (Atom (SetProject path)) = Just $ BatchCmd.setProject path

handle (Batch (Batch.ProjectSet response))    = Just $ handleResponse response doNothing doNothing
handle (Batch (Batch.FileOpened response))    = Just $ handleResponse response success doNothing where
    success _ = do
        let uri  = response ^. Response.request . OpenFile.filePath
            status = "ok"
        liftIO $ pushStatus (convert "FileOpened") (convert uri) (convert status)

handle (Batch (Batch.FileClosed response))    = Just $ handleResponse response doNothing doNothing
handle (Batch (Batch.FileSaved response))     = Just $ handleResponse response doNothing doNothing
handle (Batch (Batch.IsSaved response))       = Just $ handleResponse response success doNothing where
   success result = do
       let uri  = response ^. Response.request . IsSaved.filePath
           status = map toUpper . show $ result ^. IsSaved.status
       liftIO $ pushStatus (convert "IsSaved") (convert uri) (convert status)

handle _ = Nothing
