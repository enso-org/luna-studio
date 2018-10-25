{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.View.PortControl where

import           Common.Prelude              hiding ((.:), (.=))
import qualified Control.Lens.Aeson          as Lens
import           Data.Aeson                  (FromJSON (parseJSON), ToJSON (toEncoding, toJSON), (.:), (.=))
import qualified Data.Aeson                  as Aeson
import           Data.Convert                (Convertible (convert))
import           NodeEditor.React.Model.Port (InPort, OutPort)
import qualified NodeEditor.React.Model.Port as Port
import           LunaStudio.Data.TypeRep     (TypeRep (TCons))
import qualified LunaStudio.Data.PortDefault as PortDefault
import           LunaStudio.Data.PortDefault (PortDefault)
import           NodeEditor.View.Key         (Key)


data Value
    = BoolValue Bool
    | IntValue  Integer
    | RealValue Double
    | TextValue Text
    deriving (Eq, Generic, Show)

data PortControlView = PortControlView
    { _cls   :: Text
    , _value :: Maybe Value
    } deriving (Eq, Generic, Show)

data PortControlsView = PortControlsView
    { _key      :: Key
    , _controls :: [PortControlView]
    }  deriving (Eq, Generic, Show)

makeLenses ''Value
makeLenses ''PortControlView
makeLenses ''PortControlsView

instance NFData   PortControlsView
instance FromJSON PortControlsView
instance ToJSON   PortControlsView where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON

instance NFData   PortControlView

instance FromJSON PortControlView where
    parseJSON = Aeson.withObject "PortControlView" $ \v -> do
        cls' <- v .: "cls"
        value' <- case cls' of
            "Bool"   -> Just . BoolValue <$> v .: "value"
            "Int"    -> Just . IntValue  <$> v .: "value"
            "Real"   -> Just . RealValue <$> v .: "value"
            "String" -> Just . TextValue <$> v .: "value"
            "Text"   -> Just . TextValue <$> v .: "value"
            _        -> pure Nothing
        return $ PortControlView cls' value'

instance ToJSON PortControlView where
    toJSON (PortControlView cls' Nothing) = Aeson.object ["cls" .= cls']
    toJSON (PortControlView cls' (Just value')) = case value' of
        BoolValue  v -> Aeson.object ["cls" .= cls', "value" .= v]
        IntValue   v -> Aeson.object ["cls" .= cls', "value" .= v]
        RealValue  v -> Aeson.object ["cls" .= cls', "value" .= v]
        TextValue  v -> Aeson.object ["cls" .= cls', "value" .= v]


instance NFData   Value
instance ToJSON   Value where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON

fromPortDefault :: PortDefault -> Maybe Value
fromPortDefault = \case
    PortDefault.Expression _ -> Nothing
    PortDefault.Constant c -> Just $ case c of
        PortDefault.IntValue val  -> IntValue val
        PortDefault.RealValue val -> RealValue val
        PortDefault.TextValue val -> TextValue $ convert val
        PortDefault.BoolValue val -> BoolValue val

mkControls :: InPort -> [PortControlView]
mkControls port =  case port ^. Port.portId of
    [Port.Arg _] -> create
    []           -> create
    _            -> []
    where
        create = case port ^. Port.state of
            Port.NotConnected    -> fromVal Nothing
            Port.WithDefault val -> fromVal $ Just val
            Port.Connected       -> []
        fromVal val = case port ^. Port.valueType of
            TCons cls'  _ -> [PortControlView (convert cls') $ join $ fmap fromPortDefault val]
            _             -> []

instance Convertible PortControlView PortDefault where
    convert (PortControlView cls' value') = case value' of
        Nothing -> PortDefault.Expression $ convert cls'
        Just (BoolValue v) -> PortDefault.Constant $ PortDefault.BoolValue v
        Just (IntValue  v) -> PortDefault.Constant $ PortDefault.IntValue  v
        Just (RealValue v) -> PortDefault.Constant $ PortDefault.RealValue v
        Just (TextValue v) -> PortDefault.Constant $ PortDefault.TextValue $ convert v

instance Convertible InPort PortControlsView where
    convert port = PortControlsView
        {- key      -} (port ^. Port.portId . to convert)
        {- controls -} (mkControls port)
