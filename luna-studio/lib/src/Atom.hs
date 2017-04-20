{-# LANGUAGE JavaScriptFFI #-}
module Atom
    ( pushNotification
    ) where
import qualified Data.List                     as List
import           GHCJS.Foreign.Callback
import           GHCJS.Marshal.Pure            (pFromJSVal)
import           Common.Data.Notification
import           Common.Prelude
import           Text.Read                     (readMaybe)


foreign import javascript safe "atomCallback.pushNotification($1, $2)"
  pushNotification' :: Int -> JSString -> IO ()


pushNotification :: Notification -> IO ()
pushNotification  = do
    num <- (^. notificationType)
    msg <- (^. notificationMsg)
    return $ pushNotification' (fromEnum num) (convert msg)
