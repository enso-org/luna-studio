{-# LANGUAGE DeriveAnyClass #-}

module Luna.Data.Notification where

import           Luna.Prelude

data NotificationType = FatalError
                      | Error
                      | Warning
                      deriving (Bounded, Eq, Enum, Generic, NFData, Read, Show, Typeable)

data Notification = Notification { _notificationType :: NotificationType
                                 , _notificationMsg  :: String } deriving (Generic, NFData, Read, Show, Typeable)
makeLenses ''Notification
