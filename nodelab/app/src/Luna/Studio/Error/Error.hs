
{-# LANGUAGE DeriveAnyClass #-}

module Luna.Studio.Error.Error where

import GHCJS.Types (JSVal)
import           GHCJS.Marshal.Pure (PToJSVal (..))
import           Luna.Studio.Prelude

data NotificationType = FatalError
                      | Error
                      | Warning
                      deriving (Bounded, Eq, Enum, Generic, NFData, Read, Show, Typeable)

data Notification = Notification { _notificationType :: NotificationType
                                 , _notificationMsg  :: String } deriving (Generic, NFData, Read, Show, Typeable)
makeLenses ''Notification
