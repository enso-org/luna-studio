{-# LANGUAGE OverloadedStrings #-}
module JS.GraphLocation (
    saveLocation,
    loadLocation
) where

import           Data.Aeson                  (decode, encode)
import           Data.ByteString.Lazy.Char8  as ByteString
import           JavaScript.Web.Storage      (getItem, localStorage, setItem)
import           Luna.Studio.Prelude

import qualified Luna.Studio.Batch.Workspace as Workspace



key :: JSString
key = "lastLocation"

saveLocation :: Workspace.UIGraphLocation -> IO ()
saveLocation location = do
    let payload = convert . ByteString.unpack $ encode location
    setItem key payload localStorage

loadLocation :: IO (Maybe Workspace.UIGraphLocation)
loadLocation = do
    payload <- getItem key localStorage
    return $ decode . ByteString.pack . convert =<< payload
