{-# LANGUAGE DeriveAnyClass #-}
module TextEditor.Event.Batch where

import           Common.Prelude
import           Data.Aeson                           (ToJSON)

import qualified LunaStudio.API.Atom.CloseFile        as CloseFile
import qualified LunaStudio.API.Atom.GetBuffer        as GetBuffer
import qualified LunaStudio.API.Atom.IsSaved          as IsSaved
import qualified LunaStudio.API.Atom.Lexer            as Lexer
import qualified LunaStudio.API.Atom.OpenFile         as OpenFile
import qualified LunaStudio.API.Atom.SaveFile         as SaveFile
import qualified LunaStudio.API.Atom.SetProject       as SetProject
import qualified LunaStudio.API.Atom.Substitute       as Substitute
import qualified LunaStudio.API.Control.EmpireStarted as EmpireStarted


data Event = UnknownEvent String
           | ConnectionDropped
           | ConnectionOpened
           | EmpireStarted                       EmpireStarted.Status

           | IsSaved                                   IsSaved.Response
           | ProjectSet                             SetProject.Response
           | FileClosed                              CloseFile.Response
           | FileOpened                               OpenFile.Response
           | FileSaved                                SaveFile.Response
           | LexerUpdate                                 Lexer.Update
           | BufferGetResponse                       GetBuffer.Response
           | SubstituteResponse                     Substitute.Response
           | SubstituteUpdate                       Substitute.Update
           deriving (Eq, Show, Generic, NFData)

instance ToJSON Event
