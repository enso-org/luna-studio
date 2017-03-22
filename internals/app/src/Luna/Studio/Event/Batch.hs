{-# LANGUAGE DeriveAnyClass #-}
module Luna.Studio.Event.Batch where

import           Data.Aeson                             (ToJSON)
import           Luna.Studio.Prelude

import qualified Empire.API.Atom.GetBuffer              as GetBuffer
import qualified Empire.API.Atom.Substitute             as Substitute
import qualified Empire.API.Atom.CloseFile              as CloseFile
import qualified Empire.API.Atom.OpenFile               as OpenFile
import qualified Empire.API.Atom.SaveFile               as SaveFile
import qualified Empire.API.Atom.SetProject             as SetProject
import qualified Empire.API.Control.EmpireStarted       as EmpireStarted
import qualified Empire.API.Graph.CollaborationUpdate   as CollaborationUpdate
import qualified Empire.API.Graph.Redo                  as Redo
import qualified Empire.API.Graph.Undo                  as Undo
import qualified Empire.API.Project.CreateProject       as CreateProject
import qualified Empire.API.Project.ExportProject       as ExportProject
import qualified Empire.API.Project.ImportProject       as ImportProject
import qualified Empire.API.Project.ListProjects        as ListProjects
import qualified Empire.API.Project.OpenProject         as OpenProject


data Event = UnknownEvent String
           | CollaborationUpdate           CollaborationUpdate.Update
           | ConnectionDropped
           | ConnectionOpened
           | EmpireStarted                       EmpireStarted.Status
           | RedoResponse                                 Redo.Response
           | UndoResponse                                 Undo.Response

           | ProjectCreated                      CreateProject.Response
           | ProjectCreatedUpdate                CreateProject.Update
           | ProjectExported                     ExportProject.Response
           | ProjectImported                     ImportProject.Response
           | ProjectList                          ListProjects.Response
           | ProjectOpened                         OpenProject.Response
           | ProjectOpenedUpdate                   OpenProject.Update

           | ProjectSet                             SetProject.Response
           | FileClosed                              CloseFile.Response
           | FileOpened                               OpenFile.Response
           | FileSaved                                SaveFile.Response
           | BufferGetResponse                       GetBuffer.Response
           | SubstituteResponse                     Substitute.Response
           | SubstituteUpdate                       Substitute.Update
           deriving (Eq, Show, Generic, NFData)

instance ToJSON Event
