{-# LANGUAGE DataKinds #-}

module Luna.Studio.Data.Shader (
    ShaderBox(..)
  , Location(..)
  , createShaderBox
  ) where

import           Development.Placeholders
import           Prologue                            hiding (Bounded, s)
import           Luna.Studio.Data.Vector                        (Vector2 (Vector2))

import qualified Data.Array.Linear                   as A
import           Data.Array.Linear.Color.Class
import           Data.Maybe                          (catMaybes, fromMaybe)
import qualified Graphics.API                        as G
import           Graphics.Rendering.GLSL.SDF         (Object, diff, intersect, merge, translate)
import           Graphics.Rendering.GLSL.SDF.Figures
import           Graphics.Shading.Flat
import           Graphics.Shading.Material
import           Graphics.Shading.Pattern
import qualified Language.GLSL                       as GLSL
import qualified Language.GLSL.Builder               as GLSL
import           Math.Space.Metric.Bounded


type Vector = Vector2 Double
type Size   = Vector
type Shader = String

data Bound = Bound { __leftTop     :: Vector
                   , __rightBottom :: Vector
                   } deriving (Show, Eq)

makeLenses ''Bound

data Location = Location { _size   :: Size
                         , __offset :: Vector
                         } deriving (Show, Eq)

makeLenses ''Location

data ShaderBox = ShaderBox { __shader   :: Shader
                           , __location :: Location
                           } deriving (Show, Eq)

makeLenses ''ShaderBox

-- helpers

toFloat :: Double -> Float
toFloat = realToFrac

toExpr :: Double -> GLSL.Expr
toExpr = GLSL.FloatConstant . toFloat

toTranslation :: G.Transformation -> Vector
toTranslation (G.Transformation _ _ dx dy _ _) = Vector2 dx dy

-- object helpers

-- TODO: change pattern to object : []; object : objects ?
mergeObjects :: [Object 2] -> Maybe (Object 2)
mergeObjects []               = Nothing
mergeObjects [object]         = Just object
mergeObjects (object:objects) = Just $ foldl merge object objects

-- TODO: check behaviour
transObject :: Double -> Double -> Object 2 -> Object 2
transObject 0.0 0.0 object = object -- TODO: the translation below overrides existing translation
transObject dx  dy  object = translate tr object where
    tr = fromListUnsafe [toExpr (-dx), toExpr (-dy), toExpr 0.0] :: A.BVec 3 GLSL.Expr

-- calc object

fromMaterial :: G.Material -> Material (Layer GLSL.Expr)
fromMaterial (G.SolidColor r g b a) = Material [ Fill . Solid $ color4 (toExpr r) (toExpr g) (toExpr b) (toExpr a) ]

fromFigure :: G.Figure -> Object 2
fromFigure (G.Square s)      = hyperrectangle (A.vec2 (toExpr s) (toExpr s) :: A.BVec 2 GLSL.Expr)
fromFigure (G.Rectangle w h) = hyperrectangle (A.vec2 (toExpr w) (toExpr h) :: A.BVec 2 GLSL.Expr)
fromFigure (G.Circle d)      = ball (toExpr d)

fromPrimitive :: G.Primitive -> Object 2
fromPrimitive (G.Primitive figure (G.Point dx dy) _) = transObject dx dy $ fromFigure figure

fromShape :: G.Shape -> Object 2
fromShape (G.Shape     primitive)     = fromPrimitive primitive
fromShape (G.Merge     shape1 shape2) = fromShape shape1 `merge`     fromShape shape2
fromShape (G.Subtract  shape1 shape2) = fromShape shape1 `diff`      fromShape shape2
fromShape (G.Intersect shape1 shape2) = fromShape shape1 `intersect` fromShape shape2

fromSurface :: G.Surface -> Object 2
fromSurface (G.ShapeSurface shape) = fromShape shape
fromSurface G.PolygonSurface       = $notImplemented
fromSurface G.NumbsSurface         = $notImplemented

fromSurfaces :: [G.Surface] -> Maybe (Object 2)
fromSurfaces surfaces = mergeObjects $ fromSurface <$> surfaces

fromGeoComponent :: G.GeoComponent -> Maybe (Object 2)
fromGeoComponent (G.GeoElem  surfaces)   = fromSurfaces surfaces
fromGeoComponent (G.GeoGroup geometries) = fromGeometries geometries

fromGeometry :: G.Geometry -> Maybe (Object 2)
fromGeometry (G.Geometry geoComp trans matMay) = go <$> fromGeoComponent geoComp where
    Vector2 dx dy = toTranslation trans
    go :: Object 2 -> Object 2
    go = appMat . transObject dx dy
    appMat :: Object 2 -> Object 2
    appMat object = case matMay of
        Just mat -> object & material .~ fromMaterial mat
        Nothing  -> object

fromGeometries :: [G.Geometry] -> Maybe (Object 2)
fromGeometries geometries = mergeObjects . catMaybes $ fromGeometry <$> geometries

createShader :: Size -> Maybe (Object 2) -> Shader
createShader objSize objectMay = fromMaybe "" $ compileObject <$> objectMay where
    compileObject :: Object 2 -> Shader
    compileObject object = fst $ GLSL.compileGLSL $ Bounded (toShaderBound objSize) object

-- size calculation

defBound :: Bound
defBound = Bound (Vector2 (-1.0) (-1.0)) (Vector2 1.0 1.0)

toLocation :: Bound -> Location
toLocation (Bound (Vector2 x1 y1) (Vector2 x2 y2)) = Location (Vector2 w h) (Vector2 sx sy) where
    w  = max 0.0 $ x2 - x1
    h  = max 0.0 $ y2 - y1
    sx = (x1 + x2) / 2.0
    sy = (y1 + y2) / 2.0

-- expandBound :: Double -> Double -> Bound -> Bound
-- expandBound 0.0 0.0 bound = bound
-- expandBound dx  dy (Bound (Vector2 x1 y1) (Vector2 x2 y2)) = Bound (Vector2 x1' y1') (Vector2 x2' y2') where
--     x1' = min x1 $ x1 + dx
--     y1' = min y1 $ y1 + dy
--     x2' = max x2 $ x2 + dx
--     y2' = max y2 $ y2 + dy

moveBound :: Double -> Double -> Bound -> Bound
moveBound 0.0 0.0 bound = bound
moveBound dx  dy (Bound (Vector2 x1 y1) (Vector2 x2 y2)) = Bound (Vector2 x1' y1') (Vector2 x2' y2') where
    x1' = x1 + dx
    y1' = y1 + dy
    x2' = x2 + dx
    y2' = y2 + dy

minCorner :: Ord a => Vector2 a -> Vector2 a -> Vector2 a
minCorner (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (min x1 x2) (min y1 y2)

maxCorner :: Ord a => Vector2 a -> Vector2 a -> Vector2 a
maxCorner (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (max x1 x2) (max y1 y2)

maxBounds :: Bound -> Bound -> Bound
maxBounds (Bound lt1 rb1) (Bound lt2 rb2) = Bound (minCorner lt1 lt2) (maxCorner rb1 rb2)

minBounds :: Bound -> Bound -> Bound
minBounds (Bound lt1 rb1) (Bound lt2 rb2) = Bound (maxCorner lt1 lt2) (minCorner rb1 rb2)

maxBoundsList :: [Bound] -> Bound
maxBoundsList []                = defBound
maxBoundsList [bound]           = bound
maxBoundsList (bound:boundList) = foldl maxBounds bound boundList

toShaderBound :: Size -> A.BVec 2 Float
toShaderBound (Vector2 x y) = A.vec2 (toFloat x) (toFloat y)

calcFigureBound :: G.Figure -> Bound
calcFigureBound (G.Square s)      = Bound (Vector2 (-s2) (-s2)) (Vector2 s2 s2) where s2 = s / 2.0
calcFigureBound (G.Rectangle w h) = Bound (Vector2 (-w2) (-h2)) (Vector2 w2 h2) where w2 = w / 2.0; h2 = h / 2.0
calcFigureBound (G.Circle d)      = Bound (Vector2 (-d)  (-d))  (Vector2 d  d)

calcPrimitiveBound :: G.Primitive -> Bound
calcPrimitiveBound (G.Primitive figure (G.Point dx dy) _) = moveBound dx dy $ calcFigureBound figure
-- calcPrimitiveBound (G.Primitive figure (G.Point dx dy) attr) = trace ("pri " <> "dx " <> show dx <> " dy " <> show dy <> " " <> show bound <> " " <> show figure) $ bound where
--     bound = moveBound dx dy $ calcFigureBound figure

calcShapeBound :: G.Shape -> Bound
calcShapeBound (G.Shape     primitive)     = calcPrimitiveBound primitive
calcShapeBound (G.Merge     shape1 shape2) = maxBounds (calcShapeBound shape1) (calcShapeBound shape2)
calcShapeBound (G.Subtract  shape1 shape2) = maxBounds (calcShapeBound shape1) (calcShapeBound shape2)
calcShapeBound (G.Intersect shape1 shape2) = minBounds (calcShapeBound shape1) (calcShapeBound shape2)

calcSurfaceBound :: G.Surface -> Bound
calcSurfaceBound (G.ShapeSurface shape) = calcShapeBound shape
calcSurfaceBound G.PolygonSurface       = $notImplemented
calcSurfaceBound G.NumbsSurface         = $notImplemented

calcSurfacesBound :: [G.Surface] -> Bound
calcSurfacesBound surfaces = maxBoundsList $ calcSurfaceBound <$> surfaces

calcGeoCompBound :: G.GeoComponent -> Bound
calcGeoCompBound (G.GeoElem  surfaces)   = calcSurfacesBound surfaces
calcGeoCompBound (G.GeoGroup geometries) = maxBoundsList $ calcGeometryBound <$> geometries

calcGeometryBound :: G.Geometry -> Bound
calcGeometryBound (G.Geometry geoComp trans _) = moveBound dx dy $ calcGeoCompBound geoComp where
    Vector2 dx dy = toTranslation trans
-- calcGeometryBound (G.Geometry geoComp trans matMay) = trace ("geo " <> "dx " <> show dx <> " dy " <> show dy <> " " <> show bound) $ bound where
--     bound = moveBound dx dy $ calcGeoCompBound geoComp
--     Vector2 dx dy = toTranslation trans

calcGeometryLocation :: G.Geometry -> Location
calcGeometryLocation = toLocation . calcGeometryBound

-- -- --

createShaderBox :: G.Geometry -> ShaderBox
createShaderBox geometry = ShaderBox (createShader (location ^. size) objMay) location where
    location = calcGeometryLocation geometry
    objMay   = fromGeometry geometry

-- tests

_createShaderBoxTest :: G.Geometry -> ShaderBox
_createShaderBoxTest geometry = ShaderBox (createShader (location ^. size) objMay) location where
    -- location = calcGeometryLocation geometry
    location = toLocation $ Bound (Vector2 0.0 0.0) (Vector2 1.0 1.0)
    objMay   = fromGeometry geometry

_testGeo :: G.Geometry
_testGeo = G.Geometry geoComp trans justMat where
    trans     = def
    justMat   = Just $ G.SolidColor 1.0 0.0 0.0 1.0
    geoComp   = G.GeoElem [surface]
    surface   = G.ShapeSurface shape
    shape     = G.Shape primitive
    primitive = G.Primitive figure def def
    figure    = G.Square 0.25

_test :: IO ()
_test = do
    let _geometry = _testGeo
        ShaderBox _shaderTxt (Location (Vector2 _ _) (Vector2 _ _)) = createShaderBox _geometry
    return ()





-- ====== old test (TODO: remove) ====== --

-- mtl1 :: Material (Layer GLSL.Expr)
-- mtl1     = Material $ [ Fill            . Solid $ color4 0.7 0.2 0.2 1.0
--                       , Border 10.0     . Solid $ color4 0.0 1.0 0.0 1.0
--                       , Shadow 10.0 2.0 . Solid $ color4 0.0 0.0 0.0 0.2
--                       ] :: Material (Layer GLSL.Expr)
--
-- mtl2 :: Material (Layer GLSL.Expr)
-- mtl2     = Material $ [ Fill            . Solid $ color4 0.6 0.6 0.6 1.0
--                       ] :: Material (Layer GLSL.Expr)
--
-- mtl3 :: Material (Layer GLSL.Expr)
-- mtl3     = Material $ [ Fill            . Solid $ color4 0.3 0.3 0.3 1.0
--                       ] :: Material (Layer GLSL.Expr)
--
--
-- myBall :: Bounded Float (Object 2)
-- myBall = Bounded (A.vec2 400 400) (ball 100.0)
--        & material .~ mtl1
--
-- testRaw :: IO ()
-- testRaw = do
--     putStrLn "HSProcessing test started."
--
--     let objBall = myBall
--         [gw', gh'] = toList $ objBall ^. bounds
--         gw = gw'/2;
--         gh = gh'/2;
--
--     let (str, u) = GLSL.compileGLSL objBall
--     putStrLn str
--
--     putStrLn "HSProcessing test finished."
