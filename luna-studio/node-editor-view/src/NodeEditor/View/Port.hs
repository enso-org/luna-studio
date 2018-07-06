module NodeEditor.View.Port where

import           Common.Prelude
import qualified Control.Lens.Aeson          as Lens
import           Data.Aeson                  (FromJSON, ToJSON (toEncoding, toJSON))
import           Data.Convert                (Convertible (convert))
import           NodeEditor.React.Model.Port (InPort, OutPort)
import qualified NodeEditor.React.Model.Port as Port
import           NodeEditor.View.Color       (RGB)
import           NodeEditor.View.PortControl (PortControlView)
import qualified NodeEditor.View.PortControl as PortControl
import           NodeEditor.View.Key         (Key)


data PortView = PortView
        { _key      :: Key
        , _color    :: RGB
        , _name     :: String
        , _typeName :: String
        , _mode     :: String
        , _controls :: [PortControlView]
        } deriving (Eq, Generic, Show)

makeLenses ''PortView

instance FromJSON PortView
instance NFData   PortView
instance ToJSON   PortView where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON

instance Convertible InPort PortView where
    convert p = PortView
        {- key      -} (p ^. Port.portId . to convert)
        {- color    -} (p ^. Port.color . to convert)
        {- name     -} (p ^. Port.name . to convert)
        {- typeName -} (p ^. Port.valueType . to toString)
        {- mode     -} (if p ^. Port.portId == [Port.Self] then "self" else "in")
        {- controls -} (PortControl.fromPort p)

instance Convertible OutPort PortView where
    convert p = PortView
        {- key      -} (p ^. Port.portId . to convert)
        {- color    -} (p ^. Port.color . to convert)
        {- name     -} (p ^. Port.name . to convert)
        {- typeName -} (p ^. Port.valueType . to toString)
        {- mode     -} def
        {- controls -} def
