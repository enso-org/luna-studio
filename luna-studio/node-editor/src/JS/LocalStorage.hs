{-# LANGUAGE OverloadedStrings #-}
module JS.LocalStorage (
    saveLocation,
    loadLocation,
    saveCamera,
    loadCamera
) where

import           Common.Prelude
import           Data.Aeson                           (FromJSON, ToJSON, decode, encode)
import           Data.ByteString.Lazy.Char8           as ByteString
import           JavaScript.Web.Storage               (getItem, localStorage, setItem)

import           LunaStudio.Data.GraphLocation        (GraphLocation)
import           NodeEditor.Data.CameraTransformation (CameraTransformation)



locationKey :: JSString
locationKey = "lastLocation"

cameraKey :: FilePath -> JSString
cameraKey path = "camera" <> convert path

saveLocation :: GraphLocation -> IO ()
saveLocation = storageSave locationKey

loadLocation :: IO (Maybe GraphLocation)
loadLocation = storageLoad locationKey

saveCamera :: FilePath -> CameraTransformation -> IO ()
saveCamera path camera = storageSave (cameraKey path) camera

loadCamera :: FilePath -> IO (Maybe CameraTransformation)
loadCamera = storageLoad . cameraKey

storageSave :: ToJSON value => JSString -> value -> IO ()
storageSave key value = do
    let payload = convert . ByteString.unpack $ encode value
    setItem key payload localStorage

storageLoad :: FromJSON value => JSString -> IO (Maybe value)
storageLoad key = do
    payload <- getItem key localStorage
    return $ decode . ByteString.pack . convert =<< payload
