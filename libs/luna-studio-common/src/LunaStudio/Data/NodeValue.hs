{-# LANGUAGE TupleSections #-}
module LunaStudio.Data.NodeValue where

import qualified Data.Aeson                 as Aeson
import           Data.Binary                (Binary)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import           Data.UUID.Types            (UUID)
import           LunaStudio.Data.Error      (Error)
import           LunaStudio.Data.TypeRep    (TypeRep, toConstructorRep)
import           Prologue                   hiding (Text, TypeRep)


type VisualizerName    = Text
type VisualizerPath    = Text
type VisualizerMatcher = TypeRep -> IO (Maybe VisualizerPath)
type Visualizer        = (VisualizerName, VisualizerPath)
type VisualizationData = [Text]
type VisualizationId   = UUID

transformJSVisualizerMatcher :: MonadIO m => (String -> m String) -> TypeRep -> m (Maybe VisualizerPath)
transformJSVisualizerMatcher f r = case toConstructorRep r of
    Nothing -> return Nothing
    Just r' -> Aeson.decode . BS.pack <$> f (BS.unpack $ Aeson.encode r')

fromJSVisualizersMap :: Map String (String -> IO String) -> Map VisualizerName VisualizerMatcher
fromJSVisualizersMap = Map.fromList . map convertEntry . Map.toList where
    convertEntry (k, v) = (convert k, transformJSVisualizerMatcher v)

applyType :: MonadIO m => TypeRep -> Map VisualizerName VisualizerMatcher -> m (Map VisualizerName VisualizerPath)
applyType tpe = fmap (Map.fromList . catMaybes) . mapM applyToEntry . Map.toList where
    applyToEntry (k, f) = (k,) `fmap2` liftIO (f tpe)

type ShortValue = Text

data VisualizationValue = Value Text
                        | StreamStart
                        | StreamDataPoint Text
                        deriving (Eq, Generic, NFData, Show)

data NodeValue = NodeValue       ShortValue (Maybe VisualizationValue)
               | NodeError       Error
               deriving (Eq, Generic, NFData, Show)


makePrisms ''NodeValue
makePrisms ''VisualizationValue
instance Binary NodeValue
instance Binary VisualizationValue
