module Empire.Commands.Graphics
    ( --fromGraphics
    -- , fromLayer
    -- , fromGeometry
    -- , fromGeoComponent
    -- , fromSurface
    -- , fromShape
    -- , fromPrimitive
    -- , fromFigure
    -- , fromMaterial
    ) where

import           Prologue

import           Graphics.API

-- TODO use newer visualization-api
-- fromGraphics :: Graphics -> Graphics
-- fromGraphics = id
--
-- fromLayer :: Layer -> Graphics
-- fromLayer = convert
--
-- fromGeometry :: Geometry -> Graphics
-- fromGeometry = convert . geometryToLayer'
--
-- fromGeoComponent :: GeoComponent -> Graphics
-- fromGeoComponent = convert . geoComponentToLayer'
--
-- fromSurface :: Surface -> Graphics
-- fromSurface = convert . geoComponentToLayer' . convert
--
-- fromShape :: Shape -> Graphics
-- fromShape = convert . geoComponentToLayer' . convert
--
-- fromPrimitive :: Primitive -> Graphics
-- fromPrimitive = convert . geoComponentToLayer' . convert
--
-- fromFigure :: Figure -> Graphics
-- fromFigure = convert . geoComponentToLayer' . convert
--
-- fromMaterial :: Material -> Graphics
-- fromMaterial mat = convert . geometryToLayer' $ geometry where
--     geometry = Geometry geoComp def (Just mat)
--     geoComp  = GeoElem [ShapeSurface $ Shape $ Primitive (Square 1.0) def def]
--
-- -- internal
--
-- defaultMat :: Maybe Material
-- defaultMat = Just $ SolidColor 0.6 0.6 0.6 1.0
--
-- defaultTransf :: Transformation
-- defaultTransf = translate def 0.5 0.5
--
-- defaultTransl :: Point
-- defaultTransl = Point 0.5 0.5
--
-- geometryToLayer' :: Geometry -> Layer
-- geometryToLayer' geometry = Layer geometry (Translations [defaultTransl]) def
--
-- geoComponentToGeometry' :: GeoComponent -> Geometry
-- geoComponentToGeometry' geoComponent = Geometry geoComponent def defaultMat
--
-- geoComponentToLayer' :: GeoComponent -> Layer
-- geoComponentToLayer' = geometryToLayer' . geoComponentToGeometry'
