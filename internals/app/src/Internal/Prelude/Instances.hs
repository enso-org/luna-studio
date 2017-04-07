{-# OPTIONS_GHC -fno-warn-orphans #-}
module Internal.Prelude.Instances where

import           Data.Aeson
import           Data.Convert             (Convertible (convert))
import           Data.Default             (Default (def))
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HashMap
import           Data.JSString            (JSString)
import qualified Data.JSString            as JSString
import qualified Data.Map.Strict          as Map
import           Development.Placeholders
import           Empire.API.JSONInstances ()
import           Prologue

import           Data.UUID.Types          (UUID)

-- ======= GHCJS ===============================================================

instance Default JSString where
    def = JSString.empty

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

-- instance ToJSON b => ToJSON (HashMap AnyPortRef b) where
--     toJSON = toJSON . Map.fromList . HashMap.toList
--     {-# INLINE toJSON #-}
--
-- instance ToJSON b => ToJSON (HashMap InPortRef b) where
--     toJSON = toJSON . Map.fromList . HashMap.toList
--     {-# INLINE toJSON #-}
