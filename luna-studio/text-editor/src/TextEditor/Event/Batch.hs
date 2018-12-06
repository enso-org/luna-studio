{-# LANGUAGE DeriveAnyClass #-}
module TextEditor.Event.Batch where

import           Common.Prelude
import           Data.Aeson                           (ToJSON)

import           Common.Analytics                     (IsTrackedEvent(..))
import           Common.Data.Event                    (EventName)
import qualified LunaStudio.API.Atom.CloseFile        as CloseFile
import qualified LunaStudio.API.Atom.Copy             as Copy
import qualified LunaStudio.API.Atom.CreateProject    as CreateProject
import qualified LunaStudio.API.Atom.GetBuffer        as GetBuffer
import qualified LunaStudio.API.Atom.IsSaved          as IsSaved
import qualified LunaStudio.API.Atom.MoveProject      as MoveProject
import qualified LunaStudio.API.Atom.OpenFile         as OpenFile
import qualified LunaStudio.API.Atom.SaveFile         as SaveFile
import qualified LunaStudio.API.Atom.SetProject       as SetProject
import qualified LunaStudio.API.Atom.Substitute       as Substitute
import qualified LunaStudio.API.Control.EmpireStarted as EmpireStarted
import qualified LunaStudio.API.Control.Interpreter   as Interpreter

import LunaStudio.API.Response (ResponseOf)


data BatchEvent
        = UnknownEvent String
        | BufferGetResponse           (ResponseOf GetBuffer.Request)
        | ConnectionDropped
        | ConnectionOpened
        | CopyResponse                                 Copy.Response
        | EmpireStarted                       EmpireStarted.Status
        | FileClosed                              CloseFile.Response
        | FileOpened                               OpenFile.Response
        | FileSaved                                SaveFile.Response
        | InterpreterResponse       (ResponseOf Interpreter.Request)
        | InterpreterUpdate                     Interpreter.Update
        | IsSaved                                   IsSaved.Response
        | ProjectMove                           MoveProject.Response
        | ProjectCreated                      CreateProject.Response
        | ProjectSet                             SetProject.Response
        | SubstituteResponse         (ResponseOf Substitute.Request)
        | SubstituteUpdate                       Substitute.Update
        deriving (Show, Generic, NFData)

instance EventName BatchEvent
instance ToJSON BatchEvent
instance IsTrackedEvent BatchEvent where
    isTracked (UnknownEvent _) = False
    isTracked _                = True
