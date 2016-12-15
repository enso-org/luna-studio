{-# LANGUAGE DeriveAnyClass #-}

module Empire.API.Data.Port where

import           Control.DeepSeq              (NFData)
import           Data.Binary                  (Binary)
import           Prologue

import           Empire.API.Data.DefaultValue (PortDefault)
import           Empire.API.Data.ValueType    (ValueType)

data InPort  = Self | Arg Int        deriving (Generic, Show, Eq, Read, NFData)
data OutPort = All  | Projection Int deriving (Generic, Show, Eq, Read, NFData)

instance Binary InPort
instance Binary OutPort

data PortId = InPortId InPort | OutPortId OutPort deriving (Generic, Show, Read, Eq, NFData)

instance Ord PortId where
  (InPortId  _) `compare` (OutPortId _) = LT
  (OutPortId _) `compare` (InPortId  _) = GT
  (InPortId  a) `compare` (InPortId  b) = a `compare` b
  (OutPortId a) `compare` (OutPortId b) = a `compare` b

instance Ord InPort where
  Self `compare` Self = EQ
  Self `compare` (Arg _) = LT
  (Arg _) `compare` Self = GT
  (Arg a) `compare` (Arg b) = a `compare` b

instance Ord OutPort where
  All            `compare` All            = EQ
  All            `compare` (Projection _) = LT
  (Projection _) `compare` All            = GT
  (Projection a) `compare` (Projection b) = a `compare` b

data PortState = NotConnected | Connected | WithDefault PortDefault deriving (Show, Eq, Generic)

data Port = Port { _portId     :: PortId
                 , _name       :: String
                 , _valueType  :: ValueType
                 , _state      :: PortState
                 } deriving (Show, Eq, Generic)

makeLenses ''Port
makePrisms ''PortState
instance Binary PortId
instance Binary Port
instance Binary PortState
