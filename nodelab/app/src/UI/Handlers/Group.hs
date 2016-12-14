module UI.Handlers.Group where

import           Luna.Studio.Prelude

import           Luna.Studio.Data.Vector
import           Data.HMap.Lazy               (TypeKey (..))

import           Object.Widget                (WidgetId, ResizableWidget, resizeWidget)
import qualified Object.Widget.Group          as Model
import           Luna.Studio.Commands.Command    (Command)
import qualified Luna.Studio.Commands.UIRegistry as UICmd
import qualified Luna.Studio.State.UIRegistry    as UIRegistry

import           UI.Generic                   (defaultResize)

newtype WidgetResizedHandler = WidgetResizedHandler (WidgetId -> Vector2 Double -> Command UIRegistry.State ())
triggerWidgetResized :: WidgetId -> Vector2 Double -> Command UIRegistry.State ()
triggerWidgetResized wid vec = do
    let key = TypeKey :: TypeKey WidgetResizedHandler
    maybeHandler <- UICmd.handler wid key
    withJust maybeHandler $ \(WidgetResizedHandler handler) -> handler wid vec

instance ResizableWidget Model.Group where
    resizeWidget wid vec model = do
        defaultResize wid vec model
        triggerWidgetResized wid vec
