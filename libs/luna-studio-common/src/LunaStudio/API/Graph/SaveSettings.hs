module LunaStudio.API.Graph.SaveSettings where

import qualified Control.Lens.Aeson            as Lens
import           Data.Aeson.Types              (FromJSON (..), ToJSON (..))
import           Data.Binary                   (Binary (..))
import qualified LunaStudio.API.Graph.Request  as G
import qualified LunaStudio.API.Topic          as T
import           LunaStudio.Data.GraphLocation (GraphLocation)
import           LunaStudio.Data.Project       (LocationSettings)
import           Prologue                      hiding (TypeRep)


data Request = Request
    { _location        :: GraphLocation
    , _settings        :: LocationSettings
    } deriving (Eq, Generic, Show)

makeLenses ''Request

instance Binary Request
instance NFData Request
instance FromJSON Request where parseJSON = Lens.parse
instance ToJSON Request where
    toJSON     = Lens.toJSON
    toEncoding = Lens.toEncoding
instance G.GraphRequest Request where location = location

instance T.MessageTopic Request where
    topic = "empire.graph.saveSettings"
