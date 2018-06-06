{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.Prelude.Instances where

import           Data.Aeson
import           Data.Aeson.Types             (FromJSON, ToJSON)
import           Data.Convert                 (Convertible (convert))
import           Data.Default                 (Default (def))
import           Data.Hashable                (Hashable)
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as HashMap
import           Data.JSString                (JSString)
import qualified Data.JSString                as JSString
import qualified Data.JSString.Text           as JSString
import           Development.Placeholders
import           Prologue
import           React.Flux
import           React.Flux.Store             (ReactStoreRef)

import           LunaStudio.Data.Breadcrumb   (Breadcrumb, BreadcrumbItem)
import           LunaStudio.Data.NodeLoc      (NodeLoc, NodePath)
import           LunaStudio.Data.Port         (InPortIndex, OutPortIndex)
import           LunaStudio.Data.PortRef      (AnyPortRef, InPortRef, OutPortRef)

-- ======= React.Flux ==========================================================

instance Eq (ReactStore a) where _ == _ = True
instance NFData a => NFData (ReactStoreRef a)
instance NFData a => NFData (ReactStore a)

instance FromJSON KeyboardEvent where parseJSON = $(placeholder "Read KeyboardEvent")

-- ======= GHCJS ===============================================================

instance Default JSString where
    def = JSString.empty

instance Convertible Text JSString where
    convert = JSString.textToJSString

instance Convertible JSString Text where
    convert = JSString.textFromJSString

instance Convertible String JSString where
    convert = JSString.pack

instance Convertible JSString String where
    convert = JSString.unpack

-- ======= Data.HashMap ========================================================

-- instance (FromJSON k, FromJSON v) => FromJSON (HashMap k v) where
--     parseJSON =  fmap HashMap.fromList . parseJSON
-- instance (ToJSON k, ToJSON v) => ToJSON (HashMap k v) where
--     toJSON = toJSON . HashMap.toList
instance Default (HashMap a b) where def = HashMap.empty
instance Hashable a => Hashable (Breadcrumb a)
instance Hashable AnyPortRef
instance Hashable BreadcrumbItem
instance Hashable InPortIndex
instance Hashable InPortRef
instance Hashable NodeLoc
instance Hashable NodePath
instance Hashable OutPortIndex
instance Hashable OutPortRef
