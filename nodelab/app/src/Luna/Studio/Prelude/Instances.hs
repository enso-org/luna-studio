{-# OPTIONS_GHC -fno-warn-orphans #-}
module Luna.Studio.Prelude.Instances where

import           Data.Aeson
import           Data.Convert             (Convertible (convert))
import           Data.Default             (Default (def))
import           Data.Hashable            (Hashable)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HashMap
import           Data.JSString            (JSString)
import qualified Data.JSString            as JSString
import qualified Data.Map.Strict          as Map
import           Data.String              (fromString)
import           Development.Placeholders
import           Empire.API.JSONInstances ()
import           Prologue
import           React.Flux
import           React.Flux.Store         (ReactStoreRef)

import           Data.UUID.Types          (UUID)
import           Empire.API.Data.Port     (InPort, OutPort)
import           Empire.API.Data.PortRef  (AnyPortRef, InPortRef, OutPortRef)

-- ======= React.Flux ==========================================================

instance Eq (ReactStore a) where _ == _ = True
instance NFData a => NFData (ReactStoreRef a)
instance NFData a => NFData (ReactStore a)

instance ToJSON   MouseEvent where
    toJSON _ = toJSON "(MouseEvent)"
instance FromJSON MouseEvent where
    parseJSON = $notImplemented

instance ToJSON   KeyboardEvent where
    toJSON _ = toJSON "(KeyboardEvent)"
instance FromJSON KeyboardEvent where
    parseJSON = $notImplemented

instance ToJSON   WheelEvent where
    toJSON _ = toJSON "(WheelEvent)"
instance FromJSON WheelEvent where
    parseJSON = $notImplemented

instance ToJSON   TouchEvent where
    toJSON _ = toJSON "(TouchEvent)"
instance FromJSON TouchEvent where
    parseJSON = $notImplemented

instance ToJSON   Event where
    toJSON _ = toJSON "(Event)"
instance FromJSON Event where
    parseJSON = $notImplemented

-- ======= GHCJS ===============================================================

instance Convertible Text JSString where
    convert = JSString.pack . convert

instance Convertible JSString Text where
    convert = convert . JSString.unpack

instance Convertible String JSString where
    convert = JSString.pack

instance Convertible JSString String where
    convert = JSString.unpack

instance ToJSON b => ToJSON (HashMap UUID b) where
    toJSON = toJSON . Map.fromList . HashMap.toList
    {-# INLINE toJSON #-}

instance ToJSON b => ToJSON (HashMap AnyPortRef b) where
    toJSON = toJSON . Map.fromList . HashMap.toList
    {-# INLINE toJSON #-}

instance ToJSON b => ToJSON (HashMap InPortRef b) where
    toJSON = toJSON . Map.fromList . HashMap.toList
    {-# INLINE toJSON #-}

-- ======= Data.HashMap ========================================================

instance Default (HashMap a b) where def = HashMap.empty
instance Hashable InPort
instance Hashable OutPort
instance Hashable InPortRef
instance Hashable OutPortRef
instance Hashable AnyPortRef
