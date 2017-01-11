module UI.Raycaster where

import           Luna.Studio.Prelude

import           Luna.Studio.Data.Vector

import           GHCJS.Marshal.Pure      (pFromJSVal)
import           JavaScript.Array        (JSArray)
import qualified JavaScript.Array        as JSArray

import           Event.Event             (JSState)
import           Object.Widget           (SceneType (..), WidgetId (WidgetId), fromWidgetId)

foreign import javascript safe "raycaster.getMapPixelAt($1, $2)" getMapPixelAtJS :: Double -> Double -> IO JSArray

getMapPixelAt :: Position -> IO JSArray
getMapPixelAt pos = getMapPixelAtJS (pos ^. x) (pos ^. y)

foreign import javascript safe "raycaster.getObjectsInRect($2, $3, $4, $5)" getObjectsInRect' :: JSState -> Double -> Double -> Double -> Double -> JSArray

getObjectsInRect :: JSState -> Position -> Size -> [WidgetId]
getObjectsInRect jsstate pos size = map WidgetId list where
    idsJS = getObjectsInRect' jsstate (pos ^. x) (pos ^. y) (size ^. x) (size ^. y)
    list  = (pFromJSVal :: JSVal -> Int) <$> JSArray.toList idsJS

foreign import javascript safe "raycaster.widgetMatrix($1)" widgetMatrix :: Int -> IO JSArray

readObjectId :: Position -> IO (Maybe WidgetId)
readObjectId pos = do
    pixel <- getMapPixelAt pos
    let read i = fromJSVal $ JSArray.index i pixel :: IO (Maybe Int)
    maybeR <- read 0
    maybeG <- read 1
    maybeB <- read 2
    -- maybeA <- read 3
    return $ do
        r <- maybeR
        g <- maybeG
        b <- maybeB
        let oid = WidgetId $ r + 256 * g + 256 * 256 * b
        if oid == WidgetId 0
            then Nothing
            else Just oid

readWidgetMatrix :: Maybe WidgetId -> IO (Maybe [Double])
readWidgetMatrix (Just oid) = do
    worldMatrix <- widgetMatrix $ fromWidgetId oid
    let read i = fromJSVal $ JSArray.index i worldMatrix :: IO (Maybe Double)
    elems <- mapM read [0..15]
    return $ Just $ catMaybes elems
readWidgetMatrix  Nothing   = return Nothing

foreign import javascript safe "raycaster.isWorkspace($1)" whichSceneJS :: Int -> IO Bool

whichScene :: Maybe WidgetId -> IO (Maybe SceneType)
whichScene (Just oid) = do
    sceneType <- whichSceneJS $ fromWidgetId oid
    return $ Just $ if sceneType then Workspace else HUD
whichScene Nothing = return Nothing
