module Luna.Studio.Data.Matrix where

import           Data.Matrix             (Matrix)
import qualified Data.Matrix             as Matrix
import           Luna.Studio.Data.Vector (Position, Vector2, x, y)
import           Luna.Studio.Prelude

translationMatrix :: Vector2 Double -> Matrix Double
translationMatrix vec = Matrix.fromList 4 4 [ 1       , 0       , 0, 0
                                            , 0       , 1       , 0, 0
                                            , 0       , 0       , 1, 0
                                            , vec ^. x, vec ^. y, 0, 1 ]

invertedTranslationMatrix :: Vector2 Double -> Matrix Double
invertedTranslationMatrix vec = Matrix.fromList 4 4 [ 1          , 0          , 0, 0
                                                    , 0          , 1          , 0, 0
                                                    , 0          , 0          , 1, 0
                                                    , -(vec ^. x), -(vec ^. y), 0, 1 ]

scaleMatrix :: Double -> Matrix Double
scaleMatrix k = Matrix.fromList 4 4 [ k, 0, 0, 0
                                    , 0, k, 0, 0
                                    , 0, 0, 1, 0
                                    , 0, 0, 0, 1 ]

invertedScaleMatrix :: Double -> Matrix Double
invertedScaleMatrix k = Matrix.fromList 4 4 [ 1/k, 0  , 0, 0
                                            , 0  , 1/k, 0, 0
                                            , 0  , 0  , 1, 0
                                            , 0  , 0  , 0, 1 ]

homothetyMatrix :: Position -> Double -> Matrix Double
homothetyMatrix pos k = Matrix.fromList 4 4 [ k , 0 , 0, 0
                                            , 0 , k , 0, 0
                                            , 0 , 0 , 1, 0
                                            , hX, hY, 0, 1 ] where
    hX = (1 - k) * pos ^. x
    hY = (1 - k) * pos ^. y

invertedHomothetyMatrix :: Position -> Double -> Matrix Double
invertedHomothetyMatrix pos k = Matrix.fromList 4 4 [ 1/k  , 0    , 0, 0
                                                    , 0    , 1/k  , 0, 0
                                                    , 0    , 0    , 1, 0
                                                    , -hX/k, -hY/k, 0, 1 ] where
    hX = (1 - k) * pos ^. x
    hY = (1 - k) * pos ^. y


-- TODO[react]: Rename
transformMatrixToSvg :: String -> String -> String -> String
transformMatrixToSvg scale offsetX offsetY = "matrix(" <> scale <> " , 0, 0, " <> scale <> " , " <> offsetX <> " , " <> offsetY <> " )"

transformTranslateToSvg :: String -> String ->  String
transformTranslateToSvg offsetX offsetY = "matrix( 1 , 0, 0, 1, " <> offsetX <> " , " <> offsetY <> " )"

showTransformMatrixToSvg :: Show a => Matrix a -> String
showTransformMatrixToSvg matrix = (foldl (<>) "matrix3d(" $ intersperse ", " $ map show $ Matrix.toList matrix) <> ")"
