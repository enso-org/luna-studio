module Empire.Server.Atom where

import           Control.Monad.State                   (StateT)
import           Prologue                              hiding (Item)

import           Empire.Env                            (Env)
import qualified Empire.Env                            as Env

import           Empire.API.Request                    (Request (..))
import qualified Empire.API.Atom.SetProject            as SetProject
import qualified Empire.API.Atom.OpenFile              as OpenFile
import qualified Empire.API.Atom.SaveFile              as SaveFile
import qualified Empire.API.Atom.CloseFile             as CloseFile
import qualified Empire.API.Atom.GetBuffer             as GetBuffer
import qualified Empire.API.Atom.Substitute            as Substitute

import           ZMQ.Bus.Trans                         (BusT (..))

handleSetProject :: Request SetProject.Request -> StateT Env BusT ()
handleSetProject = $notImplemented

handleOpenFile :: Request OpenFile.Request -> StateT Env BusT ()
handleOpenFile = $notImplemented

handleSaveFile :: Request SaveFile.Request -> StateT Env BusT ()
handleSaveFile = $notImplemented

handleCloseFile :: Request CloseFile.Request -> StateT Env BusT ()
handleCloseFile = $notImplemented
