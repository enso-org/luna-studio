{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

module Empire.API.Data.Port where

import           Data.Binary                 (Binary)
import           Prologue                    hiding (TypeRep)

import           Data.Aeson.Types            (FromJSON, ToJSON)
import           Empire.API.Data.LabeledTree (LabeledTree)
import           Empire.API.Data.PortDefault (PortDefault)
import           Empire.API.Data.TypeRep     (TypeRep)


data InPortIndex = Self | Arg Int                             deriving (Show, Eq, Ord, NFData, Generic, Read)
data InPorts s   = InPorts { _self :: Maybe s, _args :: [s] } deriving (Default, Eq, Foldable, Functor, Generic, NFData, Show, Traversable)
type InPortId    = [InPortIndex]
makeLenses ''InPorts

data    OutPortIndex  = Projection Int deriving (Show, Eq, Ord, NFData, Generic, Read)
newtype OutPorts s    = OutPorts [s]   deriving (Default, Eq, Foldable, Functor, Generic, NFData, Show, Traversable)
type    OutPortId     = [OutPortIndex]
makeWrapped ''OutPorts

type InPortTree  a = LabeledTree InPorts  a
type OutPortTree a = LabeledTree OutPorts a

instance Binary   InPortIndex
instance ToJSON   InPortIndex
instance FromJSON InPortIndex
instance Binary   s => Binary   (InPorts s)
instance ToJSON   s => ToJSON   (InPorts s)
instance FromJSON s => FromJSON (InPorts s)

instance Binary   OutPortIndex
instance ToJSON   OutPortIndex
instance FromJSON OutPortIndex
instance Binary   s => Binary   (OutPorts s)
instance ToJSON   s => ToJSON   (OutPorts s)
instance FromJSON s => FromJSON (OutPorts s)

type instance Index   (InPorts s) = InPortIndex
type instance IxValue (InPorts s) = s
instance Ixed (InPorts s) where
    ix Self    = self . _Just
    ix (Arg i) = args . ix i

type instance Index   (OutPorts s) = OutPortIndex
type instance IxValue (OutPorts s) = s
instance Ixed (OutPorts s) where
    ix (Projection i) = wrapped . ix i


data AnyPortId = InPortId' InPortId | OutPortId' OutPortId deriving (Generic, Show, Eq, Ord, NFData)
makePrisms ''AnyPortId

data PortState = NotConnected | Connected | WithDefault PortDefault deriving (Show, Eq, Generic, NFData)

data Port i = Port
        { _portId     :: i
        , _name       :: String
        , _valueType  :: TypeRep
        , _state      :: PortState
        } deriving (Show, Eq, Generic, NFData)

type InPort  = Port InPortId
type OutPort = Port OutPortId

makeLenses ''Port
makePrisms ''PortState
instance Binary AnyPortId
instance Binary i => Binary (Port i)
instance Binary PortState

isInPort :: AnyPortId -> Bool
isInPort (InPortId' _) = True
isInPort _            = False

isOutPort :: AnyPortId -> Bool
isOutPort (OutPortId' _) = True
isOutPort _             = False

isSelf :: AnyPortId -> Bool
isSelf (InPortId' (Self:_)) = True
isSelf _                   = False

isInAll :: AnyPortId -> Bool
isInAll (InPortId' []) = True
isInAll _             = False

isArg :: AnyPortId -> Bool
isArg (InPortId' (Arg _:_)) = True
isArg _                    = False

isOutAll :: AnyPortId -> Bool
isOutAll (OutPortId' []) = True
isOutAll _              = False

isProjection :: AnyPortId -> Bool
isProjection (OutPortId' (Projection _:_)) = True
isProjection _                            = False

class PortNumber p where getPortNumber :: p -> Int
instance PortNumber InPortId where
    getPortNumber (Arg i : _) = i
    getPortNumber _           = 0
instance PortNumber OutPortId where
    getPortNumber (Projection i : _) = i
    getPortNumber _                  = 0
