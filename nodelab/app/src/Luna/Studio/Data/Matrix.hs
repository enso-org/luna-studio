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


-- TODO[react]: Rename
transformMatrixToSvg :: String -> String -> String -> String
transformMatrixToSvg scale offsetX offsetY = "matrix(" <> scale <> " , 0, 0, " <> scale <> " , " <> offsetX <> " , " <> offsetY <> " )"

transformTranslateToSvg :: Position ->  String
transformTranslateToSvg position = "matrix( 1 , 0, 0, 1, " <> offsetX <> " , " <> offsetY <> " )"
    where
      offsetX = show $ position ^. x
      offsetY = show $ position ^. y


showMatrix3dHTMLValue :: Matrix Double -> String
showMatrix3dHTMLValue matrix =
    let mx1 = Matrix.toList matrix
        nx  = fromIntegral ((round $ mx1!!12) :: Integer)
        ny  = fromIntegral ((round $ mx1!!13) :: Integer)
        mx2 = take 12 mx1 ++ nx:ny:drop 14 mx1
    in foldl (<>) "matrix3d(" (intersperse ", " $ map show mx2) <> ")"


showTranslateHTMLValue :: Matrix Double -> String
showTranslateHTMLValue matrix =
    let mx1 = Matrix.toList matrix
        s   = mx1!!0 :: Double
        nx  = show $ fromIntegral ((round $ s * mx1!!12) :: Integer)
        ny  = show $ fromIntegral ((round $ s * mx1!!13) :: Integer)
    in "translate(" <> nx <> "px, " <> ny <> "px)"
