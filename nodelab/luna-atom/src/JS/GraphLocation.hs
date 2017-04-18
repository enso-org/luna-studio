{-# LANGUAGE OverloadedStrings #-}
module JS.GraphLocation (
    saveLocation,
    loadLocation
) where

import           Data.Aeson                  (decode, encode)
import           Data.ByteString.Lazy.Char8  as ByteString
import           JavaScript.Web.Storage      (getItem, localStorage, setItem)
import           Luna.Prelude

import           Empire.API.Data.GraphLocation (GraphLocation)



key :: JSString
key = "lastLocation"

saveLocation :: GraphLocation -> IO ()
saveLocation location = do
    let payload = convert . ByteString.unpack $ encode location
    setItem key payload localStorage

loadLocation :: IO (Maybe GraphLocation)
loadLocation = do
    payload <- getItem key localStorage
    return $ decode . ByteString.pack . convert =<< payload
