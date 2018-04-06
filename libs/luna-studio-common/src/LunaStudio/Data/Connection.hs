module LunaStudio.Data.Connection where

import           Control.Arrow           ((&&&))
import           Data.Aeson.Types        (FromJSON, ToJSON)
import           Data.Binary             (Binary)
import           Data.Convert            (Convertible (convert))
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           LunaStudio.Data.NodeLoc (NodePath)
import qualified LunaStudio.Data.NodeLoc as NL
import           LunaStudio.Data.PortRef (InPortRef, OutPortRef)
import           Prologue


type ConnectionId = InPortRef
data Connection   = Connection
    { _src :: OutPortRef
    , _dst :: InPortRef
    } deriving (Eq, Generic, Show)

makeLenses ''Connection

instance Binary   Connection
instance NFData   Connection
instance FromJSON Connection
instance ToJSON   Connection

instance Convertible Connection (OutPortRef, InPortRef) where
    convert = (,) <$> view src <*> view dst

instance Convertible (OutPortRef, InPortRef) Connection where
    convert = uncurry Connection


connectionId :: Lens' Connection ConnectionId
connectionId = dst

toConnectionsMap :: [Connection] -> Map ConnectionId Connection
toConnectionsMap = fromList . fmap (view connectionId &&& id)

prependPath :: NodePath -> Connection -> Connection
prependPath p c = c & src %~ NL.prependPath p
                    & dst %~ NL.prependPath p
