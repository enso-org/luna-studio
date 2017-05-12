{-# LANGUAGE DeriveAnyClass #-}
module NodeEditor.Data.CameraTransformation where

import           Common.Prelude
import           Data.Aeson     (FromJSON (..), ToJSON (..))
import           Data.Matrix    (Matrix, fromLists, identity, toLists)

--TODO[react]: Consider if we can require those Matrices to be squared and of size 4
data CameraTransformation = CameraTransformation { _screenToLogical :: Matrix Double
                                                 , _logicalToScreen :: Matrix Double
                                                 , _lastInverse     :: Int
                                                 } deriving (Show, Eq, Generic, ToJSON, FromJSON)


instance ToJSON a => ToJSON (Matrix a) where
    toJSON = toJSON . toLists
instance FromJSON a => FromJSON (Matrix a) where
    parseJSON = fmap fromLists . parseJSON

makeLenses ''CameraTransformation

instance Default CameraTransformation where
    def = CameraTransformation (identity 4) (identity 4) 0
