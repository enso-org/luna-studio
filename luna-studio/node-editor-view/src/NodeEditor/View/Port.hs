module NodeEditor.View.Port where

import           Common.Prelude
import qualified Control.Lens.Aeson          as Lens
import           Data.Aeson                  (ToJSON (toEncoding, toJSON))
import           Data.Convert                (Convertible (convert))
import           NodeEditor.React.Model.Port (InPort, OutPort)
import qualified NodeEditor.React.Model.Port as Port



data PortView = PortView
        { _key :: String
        } deriving (Generic, Show)

makeLenses ''PortView

instance ToJSON PortView where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON

instance Convertible InPort PortView where
    convert c = PortView
        {- key        -} (c ^. Port.portId . to show)

instance Convertible OutPort PortView where
    convert c = PortView
        {- key        -} (c ^. Port.portId . to show)
        