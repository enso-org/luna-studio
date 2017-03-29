{-# LANGUAGE DeriveAnyClass #-}

module Internal.Error.Error where

import           Internal.Prelude

data NotificationType = FatalError
                      | Error
                      | Warning
                      deriving (Bounded, Eq, Enum, Generic, NFData, Read, Show, Typeable)

data Notification = Notification { _notificationType :: NotificationType
                                 , _notificationMsg  :: String } deriving (Generic, NFData, Read, Show, Typeable)
makeLenses ''Notification
