{-# LANGUAGE Strict #-}
module LunaStudio.Data.Searcher.Hint.Library where

import Prologue

import qualified LunaStudio.Data.Searcher.Hint       as Hint
import qualified LunaStudio.Data.Searcher.Hint.Class as Class

import Data.Aeson                          (ToJSON)
import Data.Binary                         (Binary)
import Data.Map.Strict                     (Map)
import Data.Text                           (Text)
import LunaStudio.Data.Searcher.Hint.Class (Class)



---------------------
-- === Library === --
---------------------

-- === Definition === --

data Library = Library
    { _functions        :: [Hint.Raw]
    , _classes          :: Map Class.Name Class
    , _globalSnippets   :: [Hint.Raw]
    , _importedSnippets :: [Hint.Raw]
    } deriving (Eq, Generic, Show)

makeLenses ''Library

instance Binary Library
instance NFData Library
instance ToJSON Library


type Name = Text
type Set  = Map Name Library

------------------
-- === Info === --
------------------

-- === Definition === --

data Info = Info
    { _name     :: Name
    , _imported :: Bool
    } deriving (Eq, Generic, Show)

makeLenses ''Info

instance Binary Info
instance NFData Info
