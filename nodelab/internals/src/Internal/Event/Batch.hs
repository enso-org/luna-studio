{-# LANGUAGE DeriveAnyClass #-}
module Internal.Event.Batch where

import           Data.Aeson                             (ToJSON)
import           Internal.Prelude

import qualified Empire.API.Atom.IsSaved                as IsSaved
import qualified Empire.API.Atom.GetBuffer              as GetBuffer
import qualified Empire.API.Atom.Substitute             as Substitute
import qualified Empire.API.Atom.CloseFile              as CloseFile
import qualified Empire.API.Atom.OpenFile               as OpenFile
import qualified Empire.API.Atom.SaveFile               as SaveFile
import qualified Empire.API.Atom.SetProject             as SetProject
import qualified Empire.API.Control.EmpireStarted       as EmpireStarted
import qualified Empire.API.Graph.CollaborationUpdate   as CollaborationUpdate


data Event = UnknownEvent String
           | CollaborationUpdate           CollaborationUpdate.Update
           | ConnectionDropped
           | ConnectionOpened
           | EmpireStarted                       EmpireStarted.Status

           | IsSaved                                   IsSaved.Response
           | ProjectSet                             SetProject.Response
           | FileClosed                              CloseFile.Response
           | FileOpened                               OpenFile.Response
           | FileSaved                                SaveFile.Response
           | BufferGetResponse                       GetBuffer.Response
           | SubstituteResponse                     Substitute.Response
           | SubstituteUpdate                       Substitute.Update
           deriving (Eq, Show, Generic, NFData)

instance ToJSON Event
