{-# LANGUAGE Strict #-}
module LunaStudio.Data.Searcher.Hint where

import Prologue

import Data.Aeson  (ToJSON)
import Data.Binary (Binary)
import Data.Text   (Text)
import Searcher.Engine.Data.Database (SearcherData (text, fixedScore))



-----------------
-- === Raw === --
-----------------


-- === Definition === --

data Raw = Raw
    { _name              :: Text
    , _documentationText :: Text
    } deriving (Eq, Generic, Show)

makeLenses ''Raw

instance Binary Raw
instance NFData Raw
instance ToJSON Raw


class SearcherData a => SearcherHint a where
    prefix        :: Getter a Text
    documentation :: Getter a Text

instance SearcherHint Text where
    prefix        = to $! const mempty
    documentation = to $! const mempty

instance SearcherData Raw where
    text       = name
    fixedScore = to $! const 1

instance SearcherHint Raw where
    prefix        = to $! const mempty
    documentation = documentationText





