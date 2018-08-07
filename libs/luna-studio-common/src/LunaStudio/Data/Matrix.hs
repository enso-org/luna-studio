module LunaStudio.Data.Matrix where

import           Control.Lens                   (makeWrapped)
import           Data.Convert                   (Convertible (..))
import           Data.Matrix                    (Matrix)
import qualified Data.Matrix                    as Matrix
import           LunaStudio.Data.Position       (Position, x, y)
import           LunaStudio.Data.ScreenPosition (ScreenPosition)
import           LunaStudio.Data.Vector2        (Dim1, Dim2, IsVector, Vector2 (Vector2), VectorOf)
import           Numeric                        (showFFloat)
import           Prologue


data CameraScale = CameraScale
    { _scale :: Double
    } deriving (Eq)

data CameraTranslate = CameraTranslate
    { fromCameraTranslate :: Vector2 Double
    } deriving (Eq, Generic)

makeWrapped ''CameraTranslate
makeLenses ''CameraScale

type instance VectorOf CameraTranslate = Vector2 Double

instance Dim1      CameraTranslate
instance Dim2      CameraTranslate
instance IsVector  CameraTranslate

type instance Item CameraTranslate = Double


instance Convertible (Matrix Double) CameraScale where
    convert matrix = CameraScale $ Matrix.toList matrix !!0

instance Convertible (Matrix Double) CameraTranslate where
    convert matrix = CameraTranslate $ Vector2 (mx!!12) (mx!!13) where
        mx = Matrix.toList matrix

translationMatrix :: Vector2 Double -> Matrix Double
translationMatrix vec = Matrix.fromList 4 4
    [ 1       , 0       , 0, 0
    , 0       , 1       , 0, 0
    , 0       , 0       , 1, 0
    , vec ^. x, vec ^. y, 0, 1 ]

invertedTranslationMatrix :: Vector2 Double -> Matrix Double
invertedTranslationMatrix vec = Matrix.fromList 4 4
    [ 1          , 0          , 0, 0
    , 0          , 1          , 0, 0
    , 0          , 0          , 1, 0
    , -(vec ^. x), -(vec ^. y), 0, 1 ]

scaleMatrix :: Double -> Matrix Double
scaleMatrix k = Matrix.fromList 4 4
    [ k, 0, 0, 0
    , 0, k, 0, 0
    , 0, 0, 1, 0
    , 0, 0, 0, 1 ]

invertedScaleMatrix :: Double -> Matrix Double
invertedScaleMatrix k = Matrix.fromList 4 4
    [ 1/k, 0  , 0, 0
    , 0  , 1/k, 0, 0
    , 0  , 0  , 1, 0
    , 0  , 0  , 0, 1 ]

homothetyMatrix :: ScreenPosition -> Double -> Matrix Double
homothetyMatrix pos k = Matrix.fromList 4 4
    [ k , 0 , 0, 0
    , 0 , k , 0, 0
    , 0 , 0 , 1, 0
    , hX, hY, 0, 1 ] where
        hX = (1 - k) * pos ^. x
        hY = (1 - k) * pos ^. y

invertedHomothetyMatrix :: ScreenPosition -> Double -> Matrix Double
invertedHomothetyMatrix pos k = Matrix.fromList 4 4
    [ 1/k  , 0    , 0, 0
    , 0    , 1/k  , 0, 0
    , 0    , 0    , 1, 0
    , -hX/k, -hY/k, 0, 1 ] where
        hX = (1 - k) * pos ^. x
        hY = (1 - k) * pos ^. y

showCameraTranslate :: CameraTranslate -> String
showCameraTranslate ct
    = "translate(" <> show (ct ^. x) <> "px, " <> show (ct ^. y) <> "px)"

showCameraScale :: CameraScale -> String
showCameraScale cs = "scale(" <> show (cs ^. scale) <> ")"

showCameraMatrix :: Matrix Double -> String
showCameraMatrix camera
    = foldl (<>) "matrix3d(" (intersperse ", " $ fmap show mx2) <> ")" where
        mx1 = Matrix.toList camera
        nx  = mx1!!12
        ny  = mx1!!13
        mx2 = take 12 mx1 <> (nx : ny : drop 14 mx1)

showNodeMatrix :: Matrix Double -> Position -> String
showNodeMatrix camera nodePos
    = foldl (<>) "matrix3d(" (intersperse ", " $ fmap show mx2) <> ")" where
        mx1    = Matrix.toList camera
        scale' = mx1!!0
        nx     = fromInteger
            (round $ mx1!!12 + (scale' * nodePos ^. x):: Integer)
        ny     = fromInteger
            (round $ mx1!!13 + (scale' * nodePos ^. y):: Integer)
        mx2    = take 12 mx1 <> (nx : ny : drop 14 mx1)

showNodeTranslate :: Matrix Double -> Position -> String
showNodeTranslate camera nodePos
    = "translate(" <> show1 nx <> "px, " <> show1 ny <> "px)" where
        mx1    = Matrix.toList camera
        scale' = mx1!!0
        nx     = mx1!!12 + (scale' * nodePos ^. x)
        ny     = mx1!!13 + (scale' * nodePos ^. y)

--          x'    = fromInteger (round $ camX + (scale * posX) :: Integer)
--          y'    = fromInteger (round $ camY + (scale * posY) :: Integer)

show1 :: Double -> String
show1 a = showFFloat (Just 0) a "" -- limit Double to two decimal numbers TODO: remove before the release
