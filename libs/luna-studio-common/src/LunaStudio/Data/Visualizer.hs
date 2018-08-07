module LunaStudio.Data.Visualizer where

import           Control.Lens               (makePrisms)
import           Control.Lens.Aeson         (parseDropUnary, toEncodingDropUnary)
import qualified Control.Lens.Aeson         as Lens
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

data VisualizerEntry = VisualizerEntry
    { name :: Maybe VisualizerName
    , path :: VisualizerPath
    } deriving (Eq, Generic, Show)

type VisualizerMatcher = TypeRep -> IO [VisualizerEntry]

data VisualizerType
    = InternalVisualizer
    | LunaVisualizer
    | ProjectVisualizer
    deriving (Eq, Generic, Show, Read)

makePrisms ''VisualizerType


data VisualizerId = VisualizerId
    { _visualizerName :: VisualizerName
    , _visualizerType :: VisualizerType
    } deriving (Eq, Generic, Show, Read)

makeLenses ''VisualizerId

instance Ord VisualizerId where
    visId1 `compare` visId2 = if typeOrd /= EQ
        then typeOrd
        else visName1 `compare` visName2 where
            compareTypes t1 t2
                | t1 == t2 = EQ
                | t1 == ProjectVisualizer = LT
                | t2 == ProjectVisualizer = GT
                | t1 == LunaVisualizer = LT
                | otherwise = GT
            typeOrd  = compareTypes
                (visId1 ^. visualizerType)
                (visId2 ^. visualizerType)
            visName1 = Text.unpack (visId1 ^. visualizerName)
            visName2 = Text.unpack (visId2 ^. visualizerName)

data Visualizer = Visualizer
    { _visualizerId      :: VisualizerId
    , _visualizerRelPath :: VisualizerPath
    } deriving (Eq, Generic, Ord, Show)

makeLenses ''Visualizer

instance Binary   Visualizer
instance Binary   VisualizerId
instance Binary   VisualizerType
instance FromJSON Visualizer      where parseJSON = parseDropUnary
instance FromJSON VisualizerEntry where parseJSON = parseDropUnary
instance FromJSON VisualizerId    where parseJSON = parseDropUnary
instance FromJSON VisualizerType  where parseJSON = parseDropUnary
instance NFData   Visualizer
instance NFData   VisualizerEntry
instance NFData   VisualizerId
instance NFData   VisualizerType
instance ToJSON   VisualizerType  where toEncoding = toEncodingDropUnary
instance ToJSON   VisualizerId    where toEncoding = toEncodingDropUnary
instance ToJSON   VisualizerEntry where toEncoding = toEncodingDropUnary
instance ToJSON   Visualizer      where toEncoding = toEncodingDropUnary


errorVisId, mdVisId, placeholderVisId :: VisualizerId
errorVisId       = VisualizerId "internal: error"       InternalVisualizer
mdVisId          = VisualizerId "base: markdown"        LunaVisualizer
placeholderVisId = VisualizerId "internal: placeholder" InternalVisualizer

transformJSVisualizerMatcher :: MonadIO m
    => (String -> m String) -> TypeRep -> m [VisualizerEntry]
transformJSVisualizerMatcher f r = case toConstructorRep r of
    Nothing -> pure def
    Just r' -> fromJust def . Aeson.decode . BS.pack
        <$> f (BS.unpack $ Aeson.encode r')

convertEntry :: VisualizerId -> VisualizerEntry -> (VisualizerId, VisualizerPath)
convertEntry k (VisualizerEntry Nothing  p) = (k, p)
convertEntry k (VisualizerEntry (Just n) p)
    = (k & visualizerName %~ Text.concat . (:[": ", n]), p)

fromJSVisualizersMap :: Map String (String -> IO String)
    -> Map VisualizerName VisualizerMatcher
fromJSVisualizersMap = fromList . fmap convertToEntry . toList where
    convertToEntry (k, v) = (convert k, transformJSVisualizerMatcher v)

applyType :: MonadIO m => TypeRep -> Map VisualizerId VisualizerMatcher
    -> m (Map VisualizerId VisualizerPath)
applyType tpe = fmap toMap . liftIO . mapM applyToEntry . toList where
    toMap = fromList . concat
    applyToEntry (k, f) = fmap2 (convertEntry k) $ f tpe

fromJSInternalVisualizersMap :: Map String String -> Map VisualizerId VisualizerPath
fromJSInternalVisualizersMap = fromList . concatMap convertJSON . toList where
    convertJSON (k, v)
        =   convertEntry (VisualizerId (convert k) InternalVisualizer)
        <$> (fromJust [] . Aeson.decode $ BS.pack v)

getMdVisualizer :: MonadIO m
    => Map VisualizerId VisualizerMatcher -> m (Maybe Visualizer)
getMdVisualizer visMap = fmap (Visualizer mdVisId) . Map.lookup mdVisId
    <$> applyType (TCons "Text" def) visMap
