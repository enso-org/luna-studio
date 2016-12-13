module Event.Keyboard where

import Luna.Studio.Prelude
import Data.Aeson (ToJSON)

data Type = Press | Down | Up deriving (Eq, Show, Generic)

data KeyMods = KeyMods { _shift :: Bool
                       , _ctrl  :: Bool
                       , _alt   :: Bool
                       , _meta  :: Bool
                       } deriving (Eq, Show, Generic)

data Event = Event { _tpe     :: Type
                   , _char    :: Char
                   , _keyMods :: KeyMods
                   } deriving (Eq, Show, Generic)

makeLenses ''Event
makeLenses ''KeyMods

instance ToJSON KeyMods
instance ToJSON Type
instance ToJSON Event

instance Default KeyMods where
    def = KeyMods False False False False

getString :: String -> Bool -> Maybe String
getString str cond = if cond then Just str else Nothing
