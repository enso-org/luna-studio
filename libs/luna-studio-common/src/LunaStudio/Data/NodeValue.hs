module LunaStudio.Data.NodeValue where

import           Control.Lens                  (makePrisms)
import           Data.Aeson.Types              (ToJSON)
import           Data.Binary                   (Binary)
import           Data.Text                     (Text)
import           LunaStudio.Data.Error         (Error)
import qualified LunaStudio.Data.Error         as Error
import           LunaStudio.Data.Visualization (VisualizationValue)
import           Prologue                      hiding (Text)


type ShortValue = Text

data NodeValue
    = NodeValue       ShortValue (Maybe VisualizationValue)
    | NodeError       (Error Error.NodeError)
    deriving (Eq, Generic, Show)

makePrisms ''NodeValue

instance Binary   NodeValue
instance NFData   NodeValue
instance ToJSON   NodeValue
