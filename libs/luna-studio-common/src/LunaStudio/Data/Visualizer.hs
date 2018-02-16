{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module LunaStudio.Data.Visualizer where

import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Types           (FromJSON, ToJSON)
import           Data.Binary                (Binary)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           LunaStudio.Data.TypeRep    (TypeRep (TCons), toConstructorRep)
import           Prologue                   hiding (Text, TypeRep)


type VisualizerName = Text
type VisualizerPath = Text

data VisualizerEntry = VisualizerEntry { name :: Maybe VisualizerName
                                       , path :: VisualizerPath
                                       } deriving (Eq, Generic, Show)

type VisualizerMatcher = TypeRep -> IO [VisualizerEntry]

data VisualizerType = InternalVisualizer
                    | LunaVisualizer
                    | ProjectVisualizer
                    deriving (Eq, Generic, Show)

makePrisms ''VisualizerType


data VisualizerId = VisualizerId { _visualizerName :: VisualizerName
                                 , _visualizerType :: VisualizerType
                                 } deriving (Eq, Generic, Show)

makeLenses ''VisualizerId

instance Ord VisualizerId where
    visId1 `compare` visId2 = if typeOrd /= EQ then typeOrd else  visName1 `compare` visName2 where
        compareTypes t1 t2 = if t1 == t2 then EQ
                        else if t1 == ProjectVisualizer then LT
                        else if t2 == ProjectVisualizer then GT
                        else if t1 == LunaVisualizer    then LT
                        else GT
        typeOrd  = compareTypes (visId1 ^. visualizerType) (visId2 ^. visualizerType)
        visName1 = Text.unpack (visId1 ^. visualizerName)
        visName2 = Text.unpack (visId2 ^. visualizerName)



data Visualizer = Visualizer { _visualizerId      :: VisualizerId
                             , _visualizerRelPath :: VisualizerPath
                             } deriving (Eq, Generic, Ord, Show)

makeLenses ''Visualizer


instance Binary   Visualizer
instance Binary   VisualizerId
instance Binary   VisualizerType
instance FromJSON Visualizer
instance FromJSON VisualizerEntry
instance FromJSON VisualizerId
instance FromJSON VisualizerType
instance NFData   Visualizer
instance NFData   VisualizerEntry
instance NFData   VisualizerId
instance NFData   VisualizerType
instance ToJSON   VisualizerType
instance ToJSON   VisualizerId
instance ToJSON   VisualizerEntry
instance ToJSON   Visualizer


errorVisId, mdVisId, placeholderVisId :: VisualizerId
errorVisId       = VisualizerId "internal: error"       InternalVisualizer
mdVisId          = VisualizerId "base: markdown"        LunaVisualizer
placeholderVisId = VisualizerId "internal: placeholder" InternalVisualizer

transformJSVisualizerMatcher :: MonadIO m => (String -> m String) -> TypeRep -> m [VisualizerEntry]
transformJSVisualizerMatcher f r = case toConstructorRep r of
    Nothing -> return def
    Just r' -> fromMaybe def . Aeson.decode . BS.pack <$> f (BS.unpack $ Aeson.encode r')

convertEntry :: VisualizerId -> VisualizerEntry -> (VisualizerId, VisualizerPath)
convertEntry k (VisualizerEntry Nothing  p) = (k, p)
convertEntry k (VisualizerEntry (Just n) p) = (k & visualizerName %~ Text.concat . (:[": ", n]), p)

fromJSVisualizersMap :: Map String (String -> IO String) -> Map VisualizerName VisualizerMatcher
fromJSVisualizersMap = Map.fromList . map convertToEntry . Map.toList where
    convertToEntry (k, v) = (convert k, transformJSVisualizerMatcher v)

applyType :: MonadIO m => TypeRep -> Map VisualizerId VisualizerMatcher -> m (Map VisualizerId VisualizerPath)
applyType tpe = fmap (Map.fromList . concat) . liftIO . mapM applyToEntry . Map.toList where
    applyToEntry (k, f) = fmap2 (convertEntry k) $ f tpe

fromJSInternalVisualizersMap :: Map String String -> Map VisualizerId VisualizerPath
fromJSInternalVisualizersMap = Map.fromList . concat . fmap convertJSON . Map.toList where
    convertJSON (k, v) = convertEntry (VisualizerId (convert k) InternalVisualizer) <$> (fromMaybe [] . Aeson.decode $ BS.pack v)

getMdVisualizer :: MonadIO m => Map VisualizerId VisualizerMatcher -> m (Maybe Visualizer)
getMdVisualizer visMap = fmap (Visualizer mdVisId) . Map.lookup mdVisId <$> applyType (TCons "Text" def) visMap
