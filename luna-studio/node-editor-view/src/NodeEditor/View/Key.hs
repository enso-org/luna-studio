module NodeEditor.View.Key where

import           Common.Prelude
import qualified Control.Lens.Aeson                   as Lens
import           Data.Aeson                           (FromJSON (parseJSON), ToJSON (toEncoding, toJSON))
import qualified Data.Aeson                           as Aeson
import           Data.Convert                         (Convertible (convert))
import           Data.UUID.Types.Internal             (UUID)
import           LunaStudio.Data.NodeLoc              (NodeLoc)
import           LunaStudio.Data.Connection           (ConnectionId)
import           LunaStudio.Data.Port                 (AnyPortId (InPortId', OutPortId'), InPortId, OutPortId)
import           NodeEditor.React.Model.Visualization (VisualizationId, VisualizerId)
import           Prelude                              (error)


newtype Key = Key { _unKey :: String }
    deriving (Eq, Generic, Show)

makeLenses ''Key

instance FromJSON Key where
    parseJSON = fmap Key . parseJSON
instance NFData   Key
instance ToJSON   Key where
    toJSON = toJSON . _unKey

instance Convertible NodeLoc Key where convert = Key . show
instance Convertible Key NodeLoc where convert = read . _unKey

instance Convertible VisualizationId Key where convert = Key . show
instance Convertible Key VisualizationId where convert = read . _unKey

instance Convertible VisualizerId Key where convert = Key . show
instance Convertible Key VisualizerId where convert = read . _unKey

instance Convertible ConnectionId Key where convert = Key . show
instance Convertible Key ConnectionId where convert = read . _unKey

instance Convertible InPortId Key where
    convert = Key . ("i" <>) . show

instance Convertible Key InPortId where
    convert (Key ('i':key)) = read key
    convert (Key key) = error $ "Cannot parse " <> key <> " to InPortId"

instance Convertible OutPortId Key where
    convert = Key . ("o" <>) . show

instance Convertible Key OutPortId where
    convert (Key ('o':key)) = read key
    convert (Key key) = error $ "Cannot parse " <> key <> " to OutPortId"

instance Convertible AnyPortId Key where
    convert (InPortId' inPortId) = convert inPortId
    convert (OutPortId' outPortId) = convert outPortId

instance Convertible Key AnyPortId where
    convert (Key ('i':key)) = InPortId' $ read key
    convert (Key ('o':key)) = OutPortId' $ read key
    convert (Key key) = error $ "Cannot parse " <> key <> " to AnyPortId"

instance Convertible UUID String where
    convert = show