module Empire.Commands.Library
    ( withLibrary
    , listLibraries
    , createLibrary
    ) where

import           Control.Monad.Error     (throwError)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.IntMap             as IntMap
import           Empire.Prelude

import           Empire.Data.Library     (Library)
import qualified Empire.Data.Library     as Library
import           Empire.Data.Project     (Project)
import qualified Empire.Data.Project     as Project

import           Empire.API.Data.Library (LibraryId)
import           Empire.API.Data.Project (ProjectId)

import           Empire.Commands.Project (withProject)
import           Empire.Empire           (Command, Empire)
import qualified Empire.Empire           as Empire
import qualified Empire.Utils.IdGen      as IdGen

createLibrary :: ProjectId -> Maybe String -> FilePath -> Empire (LibraryId, Library)
createLibrary pid name path = withProject pid $ do
    let library = Library.make name path
    id <- insertAtNewId library
    return (id, library)

listLibraries :: ProjectId -> Empire [(LibraryId, Library)]
listLibraries pid = withProject pid $ uses Project.libs IntMap.toList

withLibrary :: ProjectId -> LibraryId -> Command Library a -> Empire a
withLibrary pid lid cmd = withProject pid $ do
    zoom (Project.libs . at lid) $ do
        libMay <- get
        notifEnv <- ask
        case libMay of
            Nothing  -> throwError $ "Library " ++ (show lid) ++ " does not exist."
            Just lib -> do
                let result = (_2 %~ Just) <$> Empire.runEmpire notifEnv lib cmd
                Empire.empire $ const $ const result

-- internal

insertAtNewId :: Library -> Command Project LibraryId
insertAtNewId library = do
    libs <- use Project.libs
    let key = IdGen.nextId libs
    Project.libs . at key ?= library
    return key
