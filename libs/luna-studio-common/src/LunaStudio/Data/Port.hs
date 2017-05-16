{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

module LunaStudio.Data.Port where

import           Data.Binary                 (Binary)
import           Prologue                    hiding (TypeRep)
import           Data.Aeson.Types            (FromJSON, ToJSON)
import           LunaStudio.Data.LabeledTree (LabeledTree)
import           LunaStudio.Data.PortDefault (PortDefault)
import           LunaStudio.Data.TypeRep     (TypeRep)


data InPortIndex = Self | Arg Int                             deriving (Eq, Generic, NFData, Ord, Read, Show)
data InPorts s   = InPorts { _self :: Maybe s, _args :: [s] } deriving (Default, Eq, Foldable, Functor, Generic, NFData, Show, Traversable)
type InPortId    = [InPortIndex]
makeLenses ''InPorts

data    OutPortIndex  = Projection Int deriving (Eq, Generic, NFData, Ord, Read, Show)
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

data PortState = NotConnected | Connected | WithDefault PortDefault deriving (Eq, Generic, NFData, Show)

data Port i = Port
        { _portId     :: i
        , _name       :: Text
        , _valueType  :: TypeRep
        , _state      :: PortState
        } deriving (Eq, Generic, NFData, Show)

type InPort  = Port InPortId
type OutPort = Port OutPortId

makeLenses ''Port
makePrisms ''PortState
instance Binary AnyPortId
instance Binary i => Binary (Port i)
instance Binary PortState

isInPort :: AnyPortId -> Bool
isInPort (InPortId' _) = True
isInPort _             = False

isOutPort :: AnyPortId -> Bool
isOutPort (OutPortId' _) = True
isOutPort _              = False

isSelf :: InPortId -> Bool
isSelf (Self:_) = True
isSelf _        = False

isInAll :: InPortId -> Bool
isInAll [] = True
isInAll _  = False

isArg :: InPortId -> Bool
isArg (Arg _:_) = True
isArg _         = False

isOutAll :: OutPortId -> Bool
isOutAll [] = True
isOutAll _  = False

isProjection :: OutPortId -> Bool
isProjection (Projection _:_) = True
isProjection _                = False

withOut :: (OutPortId -> Bool) -> AnyPortId -> Bool
withOut = anyOf _OutPortId'

withIn :: (InPortId -> Bool) -> AnyPortId -> Bool
withIn = anyOf _InPortId'

class PortNumber p where
    getPortNumber :: p -> Int
instance PortNumber InPortId where
    getPortNumber (Arg i : _) = i
    getPortNumber _           = 0
instance PortNumber OutPortId where
    getPortNumber (Projection i : _) = i
    getPortNumber _                  = 0
instance PortNumber AnyPortId where
    getPortNumber (InPortId' i) = getPortNumber i
    getPortNumber (OutPortId' i) = getPortNumber i
