{-# LANGUAGE DeriveAnyClass      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LunaStudio.Data.CameraTransformation  where

import           Control.DeepSeq          (NFData)
import           Control.Lens.Aeson       (lensJSONParse, lensJSONToEncoding, lensJSONToJSON)
import           Data.Aeson               (FromJSON (..), ToJSON (..))
import           Data.Binary              (Binary (..))
import           Data.Matrix              (multStd2)
import           Data.Matrix              (Matrix, fromLists, identity, toLists)
import           LunaStudio.Data.Matrix   (invertedScaleMatrix, invertedTranslationMatrix, scaleMatrix, translationMatrix)
import           LunaStudio.Data.Position (Position, vector, x, y)
import           LunaStudio.Data.Size     (Size (Size))
import           LunaStudio.Data.Vector2  (Vector2 (Vector2), scalarProduct)
import           LunaStudio.Data.Vector2  (Vector2 (Vector2), scalarProduct)
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

getCameraForRectangle :: (Position, Position) -> Maybe Size -> CameraTransformation
getCameraForRectangle (leftTop, rightBottom) mayScreenSize = do
    let padding = Vector2 80 80
        span    = Size (rightBottom ^. vector - leftTop ^. vector + scalarProduct padding 2)
        shift   = scalarProduct (leftTop ^. vector - rightBottom ^. vector) 0.5 - leftTop ^. vector
        factor  = maybe 1 (\screenSize -> min 1 $ min (screenSize ^. x / span ^. x) (screenSize ^. y / span ^. y)) mayScreenSize
    CameraTransformation (multStd2 (invertedScaleMatrix factor) (invertedTranslationMatrix shift)) (multStd2 (translationMatrix shift) (scaleMatrix factor)) 2
