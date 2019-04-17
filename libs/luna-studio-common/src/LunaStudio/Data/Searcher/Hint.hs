{-# LANGUAGE Strict #-}
module LunaStudio.Data.Searcher.Hint where

import Prologue

import qualified Control.Lens.Aeson as LensAeson
import qualified Data.Aeson         as Aeson

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
    , _tags          :: [Text]
    , _priority      :: Maybe Int
    } deriving (Eq, Generic, Show)

makeLenses ''Raw

instance Binary Raw
instance NFData Raw

instance ToJSON Raw where
    toJSON     = LensAeson.toJSON
    toEncoding = LensAeson.toEncoding

instance FromJSON Raw where
    parseJSON = Aeson.withObject "Raw" $ \o -> do
        name     <- o Aeson..:  "name"
        doc      <- o Aeson..:  "documentation"
        tags     <- o Aeson..:? "tags" Aeson..!= mempty
        priority <- o Aeson..:? "priority"
        pure $ Raw name doc tags priority

-- === Construction === --

mk :: Text -> Raw
mk name = Raw name mempty mempty mempty

mkDocumented :: Text -> Text -> Maybe Int -> Raw
mkDocumented name doc = Raw name doc mempty
