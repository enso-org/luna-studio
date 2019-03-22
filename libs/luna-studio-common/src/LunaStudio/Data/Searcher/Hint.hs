{-# LANGUAGE Strict #-}
module LunaStudio.Data.Searcher.Hint where

import Prologue

import qualified Data.Aeson as Aeson

import Control.Lens (Getter, to)
import Data.Aeson   (ToJSON, FromJSON)
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

instance Binary   Raw
instance NFData   Raw
instance ToJSON   Raw
instance FromJSON Raw where
    parseJSON = Aeson.withObject "Raw" $ \v ->
        Raw <$> v Aeson..: "name"
            <*> v Aeson..: "documentation"
