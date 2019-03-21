{-# LANGUAGE Strict #-}
module LunaStudio.Data.Searcher.Hint.Class where

import Prologue

import qualified LunaStudio.Data.Searcher.Hint as Hint

import Data.Aeson  (ToJSON)
import Data.Binary (Binary)
import Data.Text   (Text)



-------------------
-- === Class === --
-------------------

-- === Definition === --

type Name = Text

data Class = Class
    { _constructors :: [Hint.Raw]
    , _methods      :: [Hint.Raw]
    , _snippets     :: [Hint.Raw]
    } deriving (Eq, Generic, Show)

makeLenses ''Class

instance Binary Class
instance NFData Class
instance ToJSON Class
