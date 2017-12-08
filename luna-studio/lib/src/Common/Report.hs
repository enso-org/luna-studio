{-# LANGUAGE DeriveAnyClass #-}

module Common.Report where

import           Atom                       (pushNotification)
import           Data.Aeson                 (ToJSON, toJSON)
import           Common.Analytics
import           Common.Data.Event          (EventName(eventName))
import           Common.Data.Notification   (Notification(Notification))
import qualified Common.Data.Notification   as Notification
import           Common.Prelude


data ErrorEvent = Fatal   { contents :: String }
                | Error   { contents :: String }
                | Warning { contents :: String }
        deriving (Generic, Show, ToJSON)

instance EventName ErrorEvent where
    eventName = const "LunaStudio.Error"

instance IsTrackedEvent ErrorEvent where
    eventData = toJSON

error :: MonadIO m => String -> m ()
error ev = do
    track $ Error ev
    liftIO $ pushNotification $ Notification Notification.Error ev

fatal :: MonadIO m => String -> m ()
fatal ev = do
    track $ Fatal ev
    liftIO $ pushNotification $ Notification Notification.FatalError ev

warning :: MonadIO m => String -> m ()
warning ev = do
    track $ Warning ev
    liftIO $ pushNotification $ Notification Notification.Warning ev
