{-# LANGUAGE DeriveAnyClass #-}

module Luna.Studio.Report where

import           JS.Atom                       (pushNotification)
import           Luna.Studio.Data.Notification
import           Luna.Studio.Prelude



error :: MonadIO m => String -> m ()
error = liftIO . pushNotification . Notification Error

fatal :: MonadIO m => String -> m ()
fatal = liftIO . pushNotification . Notification FatalError

warning :: MonadIO m => String -> m ()
warning = liftIO . pushNotification . Notification Warning
