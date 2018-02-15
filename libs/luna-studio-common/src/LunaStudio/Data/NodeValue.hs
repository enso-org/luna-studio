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


type VisualizerName    = Text
type VisualizerPath    = Text
type VisualizerMatcher = TypeRep -> IO [VisualizerEntry]
type Visualizer        = (VisualizerName, VisualizerPath)
type VisualizationData = [Text]
type VisualizationId   = UUID

data VisualizerEntry = VisualizerEntry { name :: Maybe VisualizerName
                                       , path :: VisualizerPath
                                       } deriving (Eq, Generic, Show)

instance NFData   VisualizerEntry
instance FromJSON VisualizerEntry
instance ToJSON   VisualizerEntry

mdVisName, errorVisName, placeholderVisName :: Text
mdVisName          = "base: markdown"
errorVisName       = "error"
placeholderVisName = "placeholder"

transformJSVisualizerMatcher :: MonadIO m => (String -> m String) -> TypeRep -> m [VisualizerEntry]
transformJSVisualizerMatcher f r = case toConstructorRep r of
    Nothing -> return def
    Just r' -> fromMaybe def . Aeson.decode . BS.pack <$> f (BS.unpack $ Aeson.encode r')

fromJSVisualizersMap :: Map String (String -> IO String) -> Map VisualizerName VisualizerMatcher
fromJSVisualizersMap = Map.fromList . map convertEntry . Map.toList where
    convertEntry (k, v) = (convert k, transformJSVisualizerMatcher v)

applyType :: MonadIO m => TypeRep -> Map VisualizerName VisualizerMatcher -> m (Map VisualizerName VisualizerPath)
applyType tpe = fmap (Map.fromList . concat) . liftIO . mapM applyToEntry . Map.toList where
    applyToEntry (k, f) = liftIO $ map (convertToEntry k) <$> f tpe
    convertToEntry k (VisualizerEntry Nothing  p) = (k, p)
    convertToEntry k (VisualizerEntry (Just n) p) = (Text.concat [k, ": ", n], p)

getMdVis :: MonadIO m => Map VisualizerName VisualizerMatcher -> m (Maybe Visualizer)
getMdVis visMap = fmap (mdVisName,) . Map.lookup mdVisName <$> applyType (TCons "Text" def) visMap

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
