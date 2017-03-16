module Empire.API.Data.Port where

import           Data.Binary                 (Binary)
import           Prologue                    hiding (TypeRep)

import           Empire.API.Data.PortDefault (PortDefault)
import           Empire.API.Data.TypeRep     (TypeRep)

data InPort  = Self | Arg Int        deriving (Generic, Show, Eq, Read, NFData)
data OutPort = All  | Projection Int deriving (Generic, Show, Eq, Read, NFData)

instance Binary InPort
instance Binary OutPort

data PortId = InPortId InPort | OutPortId OutPort deriving (Generic, Show, Read, Eq, NFData)

makePrisms ''PortId
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

data PortState = NotConnected | Connected | WithDefault PortDefault deriving (Show, Eq, Generic, NFData)

data Port = Port { _portId     :: PortId
                 , _name       :: String
                 , _valueType  :: TypeRep
                 , _state      :: PortState
                 } deriving (Show, Eq, Generic, NFData)

makeLenses ''Port
makePrisms ''PortState
instance Binary PortId
instance Binary Port
instance Binary PortState

isInPort :: PortId -> Bool
isInPort (InPortId _) = True
isInPort _            = False

isOutPort :: PortId -> Bool
isOutPort (OutPortId _) = True
isOutPort _             = False


isSelf :: PortId -> Bool
isSelf (InPortId Self) = True
isSelf _               = False

isArg :: PortId -> Bool
isArg (InPortId (Arg _)) = True
isArg _                  = False

isAll :: PortId -> Bool
isAll (OutPortId All) = True
isAll _               = False

isProjection :: PortId -> Bool
isProjection (OutPortId (Projection _)) = True
isProjection _                          = False

getPortNumber :: PortId -> Int
getPortNumber (InPortId  (Arg i))        = i
getPortNumber (OutPortId (Projection i)) = i
getPortNumber _                          = 0
