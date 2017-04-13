{-# LANGUAGE DeriveGeneric     #-}

module Empire.ResultSaver.Env where

import           Prologue

import           Data.UUID                         (UUID)
import qualified Data.UUID                         as UUID (nil)
import           Empire.API.Data.GraphLocation     (GraphLocation(..))
import           Empire.API.Data.Breadcrumb        (Breadcrumb(..))
import           Empire.ResultSaver.ProjectDump    (ProjectDump (..))

data ResultSaverState = Init
                      | ImportRequested    { _requestId' :: UUID }
                      | ProgramRequested   { _requestId  :: UUID }
                      | ProgramReceived    ProjectDump
                      | Finished           ProjectDump
                      | Error              { _errorMessage :: String }
                      deriving (Show, Generic)

makeLenses ''ResultSaverState
makePrisms ''ResultSaverState

data ResultSaverEnv = ResultSaverEnv { _graphLocation :: GraphLocation
                                     , _state         :: ResultSaverState
                                     } deriving (Show, Generic)
makeLenses ''ResultSaverEnv

instance Default ResultSaverEnv where
    def = ResultSaverEnv (GraphLocation UUID.nil 0 (Breadcrumb [])) Init

