{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Model.App (
    module NodeEditor.React.Model.App,
) where

import           Common.Prelude
import qualified LunaStudio.Data.GraphLocation      as GraphLocation
import           NodeEditor.Batch.Workspace         (Workspace)
import qualified NodeEditor.Batch.Workspace         as Workspace
import           NodeEditor.React.Model.Breadcrumbs (Breadcrumbs)
import           NodeEditor.React.Model.NodeEditor  (NodeEditor)


data App = App { _breadcrumbs       :: Breadcrumbs
               , _nodeEditor        :: NodeEditor
               , _workspace         :: Maybe Workspace
               } deriving (Default, Eq, Generic)

makeLenses ''App

mk :: Maybe FilePath -> App
mk = App def def . fmap Workspace.mk

moduleName :: Getter App (Maybe String)
moduleName = to moduleName' where
    moduleName' a = takeBaseName' . filePath <$> a ^. workspace
    filePath a = a ^. Workspace.currentLocation . GraphLocation.filePath

takeBaseName' :: FilePath -> FilePath
takeBaseName' = reverse  . takeWhile (`notElem` ['/', '\\']) . reverse
