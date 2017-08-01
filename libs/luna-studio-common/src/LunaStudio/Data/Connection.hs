{-# LANGUAGE Rank2Types #-}
module LunaStudio.Data.Connection where

import           Data.Binary             (Binary)
import           Data.Convert            (Convertible (convert))
import           LunaStudio.Data.PortRef (InPortRef, OutPortRef)
import           Prologue


type ConnectionId = InPortRef
data Connection = Connection { _src :: OutPortRef
                             , _dst :: InPortRef
                             } deriving (Eq, Generic, Show)

makeLenses ''Connection
instance Binary Connection
instance NFData Connection

instance Convertible Connection (OutPortRef, InPortRef) where
    convert = (,) <$> view src <*> view dst

instance Convertible (OutPortRef, InPortRef) Connection where
    convert = uncurry Connection
