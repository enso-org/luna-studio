{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module LunaStudio.Data.NodeValue where

import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Types           (FromJSON, ToJSON)
import           Data.Binary                (Binary)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.UUID.Types            (UUID)
import           LunaStudio.Data.Error      (Error)
import qualified LunaStudio.Data.Error      as Error
import           LunaStudio.Data.TypeRep    (TypeRep (TCons), toConstructorRep)
import           Prologue                   hiding (Text, TypeRep)

type VisualizerName = Text
type VisualizerPath = Text

data VisualizerType = InternalVisualizer
                    | ProjectVisualizer
                    deriving (Eq, Generic, Show)

makePrisms ''VisualizerType
instance Binary   VisualizerType
instance NFData   VisualizerType
instance FromJSON VisualizerType
instance ToJSON   VisualizerType


data VisualizerId = VisualizerId { _visualizerName :: VisualizerName
                                 , _visualizerType :: VisualizerType
                                 } deriving (Eq, Generic, Show)

makeLenses ''VisualizerId

instance Ord VisualizerId where
    (VisualizerId _ InternalVisualizer) `compare` (VisualizerId _ ProjectVisualizer)  = GT
    (VisualizerId _ ProjectVisualizer)  `compare` (VisualizerId _ InternalVisualizer) = LT
    visId1 `compare` visId2 = Text.unpack (visId1 ^. visualizerName) `compare` Text.unpack (visId2 ^. visualizerName)

instance Binary   VisualizerId
instance NFData   VisualizerId
instance FromJSON VisualizerId
instance ToJSON   VisualizerId

data VisualizerEntry = VisualizerEntry { name :: Maybe VisualizerName
                                       , path :: VisualizerPath
                                       } deriving (Eq, Generic, Show)

instance NFData   VisualizerEntry
instance FromJSON VisualizerEntry
instance ToJSON   VisualizerEntry


data Visualizer = Visualizer { _visualizerId      :: VisualizerId
                             , _visualizerRelPath :: VisualizerPath
                             } deriving (Eq, Generic, Ord, Show)

makeLenses ''Visualizer

instance Binary   Visualizer
instance NFData   Visualizer
instance FromJSON Visualizer
instance ToJSON   Visualizer

type VisualizationData = [Text]
type VisualizationId   = UUID
type VisualizerMatcher = TypeRep -> IO [VisualizerEntry]

mdVisId, errorVisId :: VisualizerId
mdVisId    = VisualizerId "base: markdown" InternalVisualizer
errorVisId = VisualizerId "base: error"    InternalVisualizer

transformJSVisualizerMatcher :: MonadIO m => (String -> m String) -> TypeRep -> m [VisualizerEntry]
transformJSVisualizerMatcher f r = case toConstructorRep r of
    Nothing -> return def
    Just r' -> fromMaybe def . Aeson.decode . BS.pack <$> f (BS.unpack $ Aeson.encode r')

fromJSVisualizersMap :: Map String (String -> IO String) -> Map VisualizerName VisualizerMatcher
fromJSVisualizersMap = Map.fromList . map convertEntry . Map.toList where
    convertEntry (k, v) = (convert k, transformJSVisualizerMatcher v)

applyType :: MonadIO m => TypeRep -> Map VisualizerId VisualizerMatcher -> m (Map VisualizerId VisualizerPath)
applyType tpe = fmap (Map.fromList . concat) . liftIO . mapM applyToEntry . Map.toList where
    applyToEntry (k, f) = liftIO $ map (convertToEntry k) <$> f tpe
    convertToEntry k (VisualizerEntry Nothing  p) = (k, p)
    convertToEntry k (VisualizerEntry (Just n) p) = (k & visualizerName %~ Text.concat . (:[": ", n]), p)


getMdVis :: MonadIO m => Map VisualizerId VisualizerMatcher -> m (Maybe Visualizer)
getMdVis visMap = fmap (Visualizer mdVisId) . Map.lookup mdVisId <$> applyType (TCons "Text" def) visMap

getErrorVis :: MonadIO m => Map VisualizerId VisualizerMatcher -> m (Maybe Visualizer)
getErrorVis visMap = fmap (Visualizer errorVisId) . Map.lookup errorVisId <$> applyType (TCons "Error" def) visMap

type ShortValue = Text

data VisualizationValue = Value Text
                        | StreamStart
                        | StreamDataPoint Text
                        deriving (Eq, Generic, Show)

data NodeValue = NodeValue       ShortValue (Maybe VisualizationValue)
               | NodeError       (Error Error.NodeError)
               deriving (Eq, Generic, Show)


makePrisms ''NodeValue
makePrisms ''VisualizationValue
instance Binary   NodeValue
instance NFData   NodeValue
instance ToJSON   NodeValue
instance Binary   VisualizationValue
instance NFData   VisualizationValue
instance FromJSON VisualizationValue
instance ToJSON   VisualizationValue
