{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies      #-}

module LunaStudio.Data.Port
    ( module LunaStudio.Data.Port
    , module X
    ) where

import           Data.Aeson.Types            (FromJSON, ToJSON)
import           Data.Binary                 (Binary)
import           LunaStudio.Data.LabeledTree as X (LabeledTree (LabeledTree))
import           LunaStudio.Data.PortDefault (PortDefault)
import           LunaStudio.Data.TypeRep     (TypeRep)
import           Prologue                    hiding (TypeRep, head)


data InPortIndex = Self | Head | Arg Int                                        deriving (Eq, Generic, Ord, Read, Show)
data InPorts s   = InPorts { _self :: Maybe s, _head :: Maybe s, _args :: [s] } deriving (Eq, Foldable, Functor, Generic, Show, Traversable)
type InPortId    = [InPortIndex]
makeLenses ''InPorts

data    OutPortIndex  = Projection Int deriving (Eq, Generic, Ord, Read, Show)
newtype OutPorts s    = OutPorts [s]   deriving (Default, Eq, Foldable, Functor, Generic, NFData, Show, Traversable)
type    OutPortId     = [OutPortIndex]
makeWrapped ''OutPorts

type InPortTree  a = LabeledTree InPorts  a
type OutPortTree a = LabeledTree OutPorts a

instance Binary   InPortIndex
instance NFData   InPortIndex
instance ToJSON   InPortIndex
instance FromJSON InPortIndex
instance Binary   s => Binary   (InPorts s)
instance ToJSON   s => ToJSON   (InPorts s)
instance FromJSON s => FromJSON (InPorts s)

instance Binary   OutPortIndex
instance NFData   OutPortIndex
instance ToJSON   OutPortIndex
instance FromJSON OutPortIndex
instance Binary   s => Binary   (OutPorts s)
instance ToJSON   s => ToJSON   (OutPorts s)
instance FromJSON s => FromJSON (OutPorts s)

instance Default (InPorts s)
instance NFData s => NFData (InPorts s)
type instance Index   (InPorts s) = InPortIndex
type instance IxValue (InPorts s) = s
instance Ixed (InPorts s) where
    ix Head    = head . _Just
    ix Self    = self . _Just
    ix (Arg i) = args . ix i

type instance Index   (OutPorts s) = OutPortIndex
type instance IxValue (OutPorts s) = s
instance Ixed (OutPorts s) where
    ix (Projection i) = wrapped . ix i


data AnyPortId = InPortId'  { inPortId'  :: InPortId  }
               | OutPortId' { outPortId' :: OutPortId }
               deriving (Generic, Show, Eq, Ord)
makePrisms ''AnyPortId

data PortState = NotConnected | Connected | WithDefault PortDefault deriving (Eq, Generic, Show)

data Port i = Port
        { _portId     :: i
        , _name       :: Text
        , _valueType  :: TypeRep
        , _state      :: PortState
        } deriving (Eq, Generic, Show)

type InPort  = Port InPortId
type OutPort = Port OutPortId

makeLenses ''Port
makePrisms ''PortState
instance Binary AnyPortId
instance NFData AnyPortId
instance Binary i => Binary (Port i)
instance NFData i => NFData (Port i)
instance Binary PortState
instance NFData PortState

class PortId a where
    isInPort     :: a -> Bool
    isOutPort    :: a -> Bool
    isSelf       :: a -> Bool
    isArg        :: a -> Bool
    isProjection :: a -> Bool
    isAll        :: a -> Bool

instance PortId InPortId where
    isInPort        = const True
    isOutPort       = const False
    isSelf (Self:_) = True
    isSelf _        = False
    isArg (Arg _:_) = True
    isArg _         = False
    isProjection    = const False
    isAll           = null

instance PortId OutPortId where
    isInPort  = const False
    isOutPort = const True
    isSelf    = const False
    isArg     = const False
    isProjection (Projection _:_) = True
    isProjection _                = False
    isAll     = null

instance PortId AnyPortId where
    isInPort     (InPortId'  pid) = isInPort     pid
    isInPort     (OutPortId' pid) = isInPort     pid
    isOutPort    (InPortId'  pid) = isOutPort    pid
    isOutPort    (OutPortId' pid) = isOutPort    pid
    isSelf       (InPortId'  pid) = isSelf       pid
    isSelf       (OutPortId' pid) = isSelf       pid
    isArg        (InPortId'  pid) = isArg        pid
    isArg        (OutPortId' pid) = isArg        pid
    isProjection (InPortId'  pid) = isProjection pid
    isProjection (OutPortId' pid) = isProjection pid
    isAll        (InPortId'  pid) = isAll        pid
    isAll        (OutPortId' pid) = isAll        pid

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
