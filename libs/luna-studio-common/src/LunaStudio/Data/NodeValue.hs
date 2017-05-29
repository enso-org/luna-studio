module LunaStudio.Data.NodeValue where

import qualified Data.Aeson                 as Aeson
import           Data.Binary                (Binary)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import           LunaStudio.Data.Error      (Error)
import           LunaStudio.Data.TypeRep    (TypeRep, toConstructorRep)
import           Prologue                   hiding (Text, TypeRep)


type VisualizerName    = Text
type VisualizerPath    = Text
type VisualizerMatcher = TypeRep -> IO (Maybe VisualizerPath)

transformJSVisualizerMatcher :: (String -> IO String) -> TypeRep -> IO (Maybe VisualizerPath)
transformJSVisualizerMatcher f r = case toConstructorRep r of
    Nothing -> return Nothing
    Just r' -> Aeson.decode . BS.pack <$> f (BS.unpack $ Aeson.encode r')

fromJSVisualizersMap :: Map String (String -> IO String) -> Map Text (TypeRep -> IO (Maybe VisualizerPath))
fromJSVisualizersMap = Map.fromList . map convertEntry . Map.toList where
    convertEntry (k, v) = (convert k, transformJSVisualizerMatcher v)


type ShortValue = Text

data VisualizationValue = JsonValue String
                        | HtmlValue String
                        deriving (Eq, Generic, NFData, Show)

data NodeValue = NodeValue ShortValue [VisualizationValue]
               | NodeError Error
               deriving (Eq, Generic, NFData, Show)

data NodeVisualization = NodeVisualization { _visualizerName     :: VisualizerName
                                           , _visualizerPath     :: VisualizerPath
                                           , _visualizationData  :: VisualizationValue
                                           } deriving (Eq, Generic, NFData, Show)

makePrisms ''NodeValue
makePrisms ''NodeVisualization
makePrisms ''VisualizationValue
instance Binary NodeValue
instance Binary VisualizationValue
