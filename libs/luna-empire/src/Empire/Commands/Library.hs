{-# LANGUAGE OverloadedStrings #-}

module Empire.Commands.Library
    ( withLibrary
    , listLibraries
    , createLibrary
    -- , getBuffer
    ) where

import           Control.Monad.Reader    hiding (liftIO)
import           Control.Monad.State     hiding (liftIO)
import qualified Data.Map                as Map
import           Empire.Prelude

import           Empire.Data.AST         (astExceptionFromException, astExceptionToException)
import           Empire.Data.Graph       (CommandState(..), userState)
import           Empire.ASTOp            (defaultClsGraph)
import           Empire.Data.Library     (Library)
import qualified Empire.Data.Library     as Library

import           Empire.Empire           (Command, Empire, zoomCommand)
import qualified Empire.Empire           as Empire

createLibrary :: Maybe String -> FilePath -> Empire Library
createLibrary name path = do
    library <- liftIO $ make name path
    userState . Empire.activeFiles . at path ?= library
    pure library

make :: Maybe String -> FilePath -> IO Library
make name path = do
    clsGraph <- defaultClsGraph
    pure $ Library.Library name path clsGraph


listLibraries :: Empire [Library]
listLibraries = do
    files <- use $ userState . Empire.activeFiles
    pure $ Map.elems files

data LibraryNotFoundException = LibraryNotFoundException FilePath
    deriving (Show)

instance Exception LibraryNotFoundException where
    toException = astExceptionToException
    fromException = astExceptionFromException

withLibrary :: FilePath -> Command Library a -> Empire a
withLibrary file cmd = do
    zoomCommand (Empire.activeFiles . at file) $ do
        CommandState pm libMay <- get
        notifEnv <- ask
        case libMay of
            Nothing  -> throwM $ LibraryNotFoundException file
            Just lib -> do
                (a, CommandState pm' st) <- liftIO $ Empire.runEmpire notifEnv (CommandState pm lib) cmd
                put $ CommandState pm' $ Just st
                pure a
