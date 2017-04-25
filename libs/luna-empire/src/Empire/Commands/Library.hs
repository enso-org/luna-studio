{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Empire.Commands.Library
    ( withLibrary
    , listLibraries
    , createLibrary
    , getBuffer
    , applyDiff
    , addLineAfter
    , substituteLine
    ) where

import           Control.Monad.Except    (throwError)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map                as Map
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.IO            as Text
import           Empire.Prelude

import           Empire.Data.Graph       as Graph (defaultGraph)
import           Empire.Data.Library     (Library)
import qualified Empire.Data.Library     as Library
import           Empire.Data.Project     (Project)
import qualified Empire.Data.Project     as Project

import           Empire.API.Data.Library (LibraryId)
import           Empire.API.Data.Project (ProjectId)

import           Empire.Empire           (Command, Empire)
import qualified Empire.Empire           as Empire
import qualified Empire.Utils.IdGen      as IdGen

createLibrary :: Maybe String -> FilePath -> Text -> Empire Library
createLibrary name path code = do
    library <- liftIO $ make name path code
    Empire.activeFiles . at path ?= library
    return library

make :: Maybe String -> FilePath -> Text -> IO Library
make name path code = Library.Library name path code <$> Graph.defaultGraph


listLibraries :: Empire [Library]
listLibraries = do
    files <- use Empire.activeFiles
    return $ Map.elems files

withLibrary :: FilePath -> Command Library a -> Empire a
withLibrary file cmd = do
    zoom (Empire.activeFiles . at file) $ do
        libMay <- get
        notifEnv <- ask
        case libMay of
            Nothing  -> throwError $ "Library " ++ (show file) ++ " does not exist."
            Just lib -> do
                let result = (_2 %~ Just) <$> Empire.runEmpire notifEnv lib cmd
                Empire.empire $ const $ const result

getBuffer :: FilePath -> Maybe (Int, Int) -> Empire Text
getBuffer path Nothing = withLibrary path $ do
    source <- use Library.code
    return source

applyDiff :: Int -> Int -> Text -> Command Library Text
applyDiff start end code = do
    currentCode <- use Library.code
    let len            = end - start
        (prefix, rest) = Text.splitAt start currentCode
        suffix         = Text.drop len rest
        newCode        = Text.concat [prefix, code, suffix]
    Library.code .= newCode
    return newCode

substituteLine :: Int -> Text -> Command Library Text
substituteLine index newLine = do
    currentCode <- use Library.code
    let codeLines = Text.lines currentCode
        newCode   = Text.unlines $ codeLines & ix index .~ newLine
    Library.code .= newCode
    return newCode

addLineAfter :: Int -> Text -> Command Library Text
addLineAfter ((+1) -> index) line = do
    currentCode <- use Library.code
    let codeLines = Text.lines currentCode
        newCode   = Text.unlines $ take index codeLines ++ [line] ++ drop index codeLines
    Library.code .= newCode
    return newCode
