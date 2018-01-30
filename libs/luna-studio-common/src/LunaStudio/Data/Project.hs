module LunaStudio.Data.Project where

import qualified Control.Lens.Aeson                   as Lens
import           Data.Aeson                           (FromJSON (parseJSON), ToJSON (toEncoding, toJSON))
import           Data.Aeson.Types                     (ToJSON)
import           Data.Binary                          (Binary)
import           Data.Binary                          (Binary (..))
import           Data.Hashable                        (Hashable)
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as HashMap
import           Data.IntMap.Lazy                     (IntMap)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.UUID.Types                      (UUID)
import           Data.Yaml                            (decodeFileEither, encodeFile)
import           LunaStudio.Data.Breadcrumb           (Breadcrumb)
import           LunaStudio.Data.CameraTransformation (CameraTransformation)
import           LunaStudio.Data.Library              (Library)
import           LunaStudio.Data.NodeValue            (Visualizer)
import           LunaStudio.Data.TypeRep              (TypeRep)
import           Prologue                             hiding (TypeRep)


type ProjectId = UUID

data Project = Project { _name     :: String
                       , _libs     :: IntMap Library
                       } deriving (Eq, Generic, Show)

makeLenses ''Project
instance Binary Project
instance NFData Project
instance ToJSON Project


--TODO: Add and handle this: _breadcrumbVisualizerPreferences :: HashMap TypeRep Visualizer
data BreadcrumbSettings = BreadcrumbSettings { _breadcrumbCameraSettings :: CameraTransformation
                                             } deriving (Eq, Generic, Show)

data ModuleSettings = ModuleSettings { _currentBreadcrumb   :: Breadcrumb Text
                                     , _typeRepToVisMap     :: HashMap TypeRep Visualizer
                                     , _breadcrumbsSettings :: Map (Breadcrumb Text) BreadcrumbSettings
                                     } deriving (Eq, Generic, Show)

--TODO: Add and handle this: _projectVisualizerPreferences :: HashMap TypeRep Visualizer
data ProjectSettings = ProjectSettings { _modulesSettings :: Map FilePath ModuleSettings
                                       } deriving (Eq, Generic, Show)

data LocationSettings = LocationSettings { _visMap   :: Maybe (HashMap TypeRep Visualizer)
                                         , _camera   :: CameraTransformation
                                         } deriving (Eq, Generic, Show)

makeLenses ''BreadcrumbSettings
makeLenses ''ModuleSettings
makeLenses ''ProjectSettings
makeLenses ''LocationSettings

instance Binary   LocationSettings
instance NFData   LocationSettings
instance FromJSON BreadcrumbSettings where parseJSON = Lens.parse
instance FromJSON ModuleSettings     where parseJSON = Lens.parse
instance FromJSON ProjectSettings    where parseJSON = Lens.parse
instance FromJSON LocationSettings   where parseJSON = Lens.parse
instance ToJSON   BreadcrumbSettings where
    toJSON     = Lens.toJSON
    toEncoding = Lens.toEncoding
instance ToJSON   ModuleSettings     where
    toJSON     = Lens.toJSON
    toEncoding = Lens.toEncoding
instance ToJSON   ProjectSettings    where
    toJSON     = Lens.toJSON
    toEncoding = Lens.toEncoding
instance ToJSON   LocationSettings   where
    toJSON     = Lens.toJSON
    toEncoding = Lens.toEncoding

--FIXME[MM, LJK, PM]: We should allow sending HashMap here without convert to list
instance (Hashable k, Eq k, Binary k, Binary v) => Binary (HashMap k v) where
    put = put . HashMap.toList
    get = HashMap.fromList <$> get


getModuleSettings :: MonadIO m => FilePath -> FilePath -> m (Maybe ModuleSettings)
getModuleSettings configPath modulePath = liftIO $ either def (Map.lookup modulePath . view modulesSettings) <$> decodeFileEither configPath

updateCurrentBreadcrumbSettings :: MonadIO m => FilePath -> FilePath -> Breadcrumb Text -> m ()
updateCurrentBreadcrumbSettings configPath filePath bc = liftIO $ decodeFileEither configPath >>= encodeFile configPath . updateProjectSettings where
    createProjectSettings   = ProjectSettings $ Map.singleton filePath createModuleSettings
    updateProjectSettings   = either (const createProjectSettings) updateModuleSettings
    createModuleSettings    = ModuleSettings bc HashMap.empty def
    updateModuleSettings ps = case ps ^. modulesSettings . to (Map.lookup filePath) of
        Nothing -> ps & modulesSettings . at filePath ?~ createModuleSettings
        Just ms -> ps & modulesSettings . at filePath ?~ (ms & currentBreadcrumb .~ bc)

updateLocationSettings :: MonadIO m => FilePath -> FilePath -> Breadcrumb Text -> LocationSettings -> Breadcrumb Text -> m ()
updateLocationSettings configPath filePath bc settings currentBc = liftIO $ decodeFileEither configPath >>= encodeFile configPath . updateProjectSettings where
    createProjectSettings    = ProjectSettings $ Map.singleton filePath createModuleSettings
    updateProjectSettings    = either (const createProjectSettings) updateModuleSettings
    createModuleSettings     = ModuleSettings currentBc (fromMaybe mempty $ settings ^. visMap) $ Map.singleton bc createBreadcrumbSettings
    updateModuleSettings' ms = do
        let visMap' = fromMaybe (ms ^. typeRepToVisMap) $ settings ^. visMap
        ModuleSettings currentBc visMap' $ Map.insert bc createBreadcrumbSettings $ ms ^. breadcrumbsSettings
    updateModuleSettings  ps = ps & modulesSettings . at filePath %~ Just . maybe createModuleSettings updateModuleSettings'
    createBreadcrumbSettings = BreadcrumbSettings $ settings ^. camera
