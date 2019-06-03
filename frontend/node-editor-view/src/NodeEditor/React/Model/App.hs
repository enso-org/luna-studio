{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Model.App (
    module NodeEditor.React.Model.App,
) where

import Common.Prelude

import qualified LunaStudio.Data.GraphLocation      as GraphLocation
import qualified NodeEditor.Batch.Workspace         as Workspace
import qualified Path

import NodeEditor.Batch.Workspace         (Workspace)
import NodeEditor.React.Model.Breadcrumbs (Breadcrumbs)
import NodeEditor.React.Model.NodeEditor  (NodeEditor)
import Path                               (Path, Rel, File)


data App = App { _breadcrumbs       :: Breadcrumbs
               , _nodeEditor        :: NodeEditor
               , _workspace         :: Maybe Workspace
               } deriving (Default, Eq, Generic)

makeLenses ''App

mk :: Maybe (Path Rel File) -> App
mk = App def def . fmap Workspace.mk

moduleName :: Getter App (Maybe String)
moduleName = to moduleName' where
    moduleName' a = takeBaseName' . Path.toFilePath . filePath <$> a ^. workspace
    filePath a = a ^. Workspace.currentLocation . GraphLocation.filePath

takeBaseName' :: FilePath -> FilePath
takeBaseName' = reverse  . takeWhile (`notElem` ['/', '\\']) . reverse
