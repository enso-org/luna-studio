module UI.Scene where

import           Luna.Studio.Prelude

import qualified Data.JSString      as JSString
import           GHCJS.Foreign
import           GHCJS.Marshal.Pure (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types        (JSVal)

import           Object.Widget      (WidgetId)
import           UI.Widget          (UIContainer, UIWidget)

newtype Scene = Scene { unScene :: JSVal } deriving (PFromJSVal, PToJSVal)

instance UIWidget Scene
instance UIContainer Scene

foreign import javascript safe "{container: common.scene }"    scene    :: IO Scene
foreign import javascript safe "{container: common.sceneHUD }" sceneHUD :: IO Scene
