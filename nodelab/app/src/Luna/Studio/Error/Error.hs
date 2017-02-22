{-# LANGUAGE DeriveAnyClass #-}

module Luna.Studio.Error.Error where

import GHCJS.Types (JSVal)
import           GHCJS.Marshal.Pure (PToJSVal (..))
import           Luna.Studio.Prelude

data NotificationType = Error
                      | FatalError
                      | Warning
                      deriving (Bounded, Eq, Enum, Generic, NFData, Read, Show, Typeable)

data Notification = Notification { notificationType :: NotificationType
                                 , notificationMsg  :: [Char] } deriving (Generic, NFData, Read, Show, Typeable)


instance PToJSVal NotificationType where
  pToJSVal Error      = js_Error
  pToJSVal FatalError = js_FatalError
  pToJSVal Warning    = js_Warning

foreign import javascript unsafe "$1" js_Error      ::
        JSVal
foreign import javascript unsafe "$1" js_FatalError ::
        JSVal
foreign import javascript unsafe "$1" js_Warning    ::
        JSVal

instance (PToJSVal a, PToJSVal b) => PToJSVal (a, b) where
  pToJSVal (a, b) = arr2 (pToJSVal a) (pToJSVal b)

foreign import javascript unsafe "[$1,$2]" arr2 :: JSVal -> JSVal -> JSVal

instance PToJSVal Notification where
  pToJSVal (Notification notificationType msg) = pToJSVal (notificationType, msg)
