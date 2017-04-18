{-# LANGUAGE DeriveAnyClass #-}

module Luna.Report where

import           Atom                       (pushNotification)
import           Luna.Data.Notification
import           Luna.Prelude



error :: MonadIO m => String -> m ()
error = liftIO . pushNotification . Notification Error

fatal :: MonadIO m => String -> m ()
fatal = liftIO . pushNotification . Notification FatalError

warning :: MonadIO m => String -> m ()
warning = liftIO . pushNotification . Notification Warning
