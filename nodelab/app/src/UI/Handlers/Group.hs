module UI.Handlers.Group where

import           Luna.Studio.Prelude

import           Data.HMap.Lazy                  (TypeKey (..))
import           Luna.Studio.Data.Vector

import           Luna.Studio.Commands.Command    (Command)
import qualified Luna.Studio.Commands.UIRegistry as UICmd
import qualified Luna.Studio.State.UIRegistry    as UIRegistry
import           Object.Widget                   (ResizableWidget, WidgetId, resizeWidget)
import qualified Object.Widget.Group             as Model

import           UI.Generic                      (defaultResize)

newtype WidgetResizedHandler = WidgetResizedHandler (WidgetId -> Size -> Command UIRegistry.State ())
triggerWidgetResized :: WidgetId -> Size -> Command UIRegistry.State ()
triggerWidgetResized wid size = do
    let key = TypeKey :: TypeKey WidgetResizedHandler
    maybeHandler <- UICmd.handler wid key
    withJust maybeHandler $ \(WidgetResizedHandler handler) -> handler wid size

instance ResizableWidget Model.Group where
    resizeWidget wid vec model = do
        defaultResize wid vec model
        triggerWidgetResized wid vec
