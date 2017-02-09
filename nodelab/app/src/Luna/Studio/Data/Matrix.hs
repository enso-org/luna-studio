module Luna.Studio.Data.Matrix where

import           Data.Matrix         (Matrix)
import qualified Data.Matrix         as Matrix
import           Data.Position       (Position, Vector2, x, y)
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

translatePropertyValue :: Matrix Double -> String
translatePropertyValue matrix = "translate(" <> nx <> "px, " <> ny <> "px)"
    where mx = Matrix.toList matrix
          s  = mx!!0 :: Double
          nx = show (round $ s * mx!!12 :: Integer)
          ny = show (round $ s * mx!!13 :: Integer)

translatePropertyValue2 :: Position ->  String
translatePropertyValue2 position = "translate(" <> nx <> "px, " <> ny <> "px)"
    where nx = show $ position ^. x
          ny = show $ position ^. y

matrix3dPropertyValue :: Matrix Double -> String
matrix3dPropertyValue matrix = foldl (<>) "matrix3d(" (intersperse ", " $ map show mx2) <> ")"
    where mx1 = Matrix.toList matrix
          nx  = fromIntegral (round $ mx1!!12 :: Integer)
          ny  = fromIntegral (round $ mx1!!13 :: Integer)
          mx2 = take 12 mx1 ++ nx:ny:drop 14 mx1
