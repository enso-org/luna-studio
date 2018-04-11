module LunaStudio.Data.CameraTransformation  where

import           Control.DeepSeq          (NFData)
import qualified Control.Lens.Aeson       as Lens
import           Data.Aeson               (FromJSON (..), ToJSON (..))
import           Data.Binary              (Binary (..))
import           Data.Matrix              (Matrix, fromLists, identity, multStd2, toLists)
import           LunaStudio.Data.Matrix   (invertedScaleMatrix, invertedTranslationMatrix, scaleMatrix, translationMatrix)
import           LunaStudio.Data.Position (Position, vector, x, y)
import           LunaStudio.Data.Size     (Size (Size))
import           LunaStudio.Data.Vector2  (Vector2 (Vector2), scalarProduct)
import           Prologue                 hiding (span)


--TODO[react]: Consider if we can require those Matrices to be squared and of size 4
data CameraTransformation = CameraTransformation
    { _screenToLogical :: Matrix Double
    , _logicalToScreen :: Matrix Double
    , _lastInverse     :: Int
    } deriving (Eq, Generic, Show)

makeLenses ''CameraTransformation

instance Binary a => Binary (Matrix a) where
    put = put . toLists
    get = fromLists <$> get

instance Binary CameraTransformation
instance NFData CameraTransformation

instance (ToJSON a, Show a) => ToJSON (Matrix a) where
    toJSON = toJSON . show . toLists
instance (FromJSON a, Read a) => FromJSON (Matrix a) where
    parseJSON json = do
        (s :: String) <- parseJSON json
        either fail pure $ fromLists <$> tryReads s

instance ToJSON CameraTransformation where
    toJSON     = Lens.toJSON
    toEncoding = Lens.toEncoding
instance FromJSON CameraTransformation where parseJSON = Lens.parse

instance Default CameraTransformation where
    def = CameraTransformation (identity 4) (identity 4) 0

getCameraForRectangle :: (Position, Position) -> Maybe Size -> CameraTransformation
getCameraForRectangle (leftTop, rightBottom) mayScreenSize = do
    let lTopV      = leftTop     ^. vector
        rBotV      = rightBottom ^. vector
        padding    = Vector2 80 80
        span       = Size (rBotV - lTopV + scalarProduct padding 2)
        shift      = scalarProduct (lTopV - rBotV) 0.5 - lTopV
        toFactor s = min 1 $ min (s ^. x / span ^. x) (s ^. y / span ^. y)
        factor     = maybe 1 toFactor mayScreenSize
        sToL       = multStd2
            (invertedScaleMatrix factor)
            (invertedTranslationMatrix shift)
        lToS       = multStd2
            (translationMatrix shift)
            (scaleMatrix factor)
    CameraTransformation sToL lToS 2
