{-# LANGUAGE Strict #-}
module LunaStudio.Data.Searcher.Hint where

import Prologue

import Control.Lens (Getter, to)
import Data.Aeson   (ToJSON)
import Data.Binary  (Binary)
import Data.Text    (Text)


-----------------
-- === Raw === --
-----------------

-- === Definition === --

data Raw = Raw
    { _name          :: Text
    , _documentation :: Text
    } deriving (Eq, Generic, Show)

makeLenses ''Raw

instance Binary Raw
instance NFData Raw
instance ToJSON Raw
