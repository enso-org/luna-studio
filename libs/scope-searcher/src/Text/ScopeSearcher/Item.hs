{-# LANGUAGE DeriveAnyClass #-}
module Text.ScopeSearcher.Item where

import           Control.DeepSeq  (NFData)
import           Control.Lens
import           Data.Aeson.Types (ToJSON)
import           Data.Binary      (Binary)
import           Data.Map.Lazy    as Map
import           Data.Text        (Text)
import           GHC.Generics     (Generic)


type Items a = Map Text (Item a)

data Item a = Element { _element :: a }
            | Group   { _items :: Items a, _element :: a }
            deriving (Show, Eq, Generic, NFData)

isElement :: Item a -> Bool
isElement (Element {}) = True
isElement _            = False

isGroup :: Item a -> Bool
isGroup (Group {}) = True
isGroup _          = False


instance Binary a => Binary (Item a)

instance ToJSON a => ToJSON (Item a)

makeLenses ''Item
makePrisms ''Item
