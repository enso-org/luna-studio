{-# LANGUAGE DeriveAnyClass      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LunaStudio.Data.CameraTransformation  where

import           Control.DeepSeq    (NFData)
import           Control.Lens.Aeson (lensJSONParse, lensJSONToEncoding, lensJSONToJSON)
import           Data.Aeson         (FromJSON (..), ToJSON (..))
import           Data.Binary        (Binary (..))
import           Data.Matrix        (Matrix, fromLists, identity, toLists)
import           Prologue

--TODO[react]: Consider if we can require those Matrices to be squared and of size 4
data CameraTransformation = CameraTransformation { _screenToLogical :: Matrix Double
                                                 , _logicalToScreen :: Matrix Double
                                                 , _lastInverse     :: Int
                                                 } deriving (Show, Eq, Generic, NFData)
makeLenses ''CameraTransformation

instance Binary a => Binary (Matrix a) where
    put = put . toLists
    get = fromLists . get

instance Binary CameraTransformation

instance (ToJSON a, Show a) => ToJSON (Matrix a) where
    toJSON = toJSON . show . toLists
instance (FromJSON a, Read a) => FromJSON (Matrix a) where
    parseJSON json = do
        (s :: String) <- parseJSON json
        either fail return $ fromLists <$> tryReads s

instance ToJSON CameraTransformation where
    toJSON     = lensJSONToJSON
    toEncoding = lensJSONToEncoding
instance FromJSON CameraTransformation where parseJSON = lensJSONParse

instance Default CameraTransformation where
    def = CameraTransformation (identity 4) (identity 4) 0
