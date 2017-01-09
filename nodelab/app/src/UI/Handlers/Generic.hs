{-# LANGUAGE ScopedTypeVariables #-}

module UI.Handlers.Generic where

import           Luna.Studio.Prelude
import           Data.HMap.Lazy               (HTMap, TypeKey (..))

import           Object.Widget                (WidgetId)
import           Luna.Studio.Action.Command    (Command)
import qualified Luna.Studio.Action.UIRegistry as UICmd
import           Luna.Studio.State.Global        (inRegistry)
import qualified Luna.Studio.State.Global        as Global
import           Luna.Studio.State.UIRegistry    (addHandler)

newtype ValueChangedHandler a = ValueChangedHandler (a -> WidgetId -> Command Global.State ())

triggerValueChanged :: Typeable a => a -> WidgetId -> Command Global.State ()
triggerValueChanged new wid = do
    let key = TypeKey :: (TypeKey (ValueChangedHandler a))
    maybeHandler <- inRegistry $ UICmd.handler wid key
    withJust maybeHandler $ \(ValueChangedHandler handler) -> handler new wid

onValueChanged :: Typeable a => (a -> WidgetId -> Command Global.State ()) -> HTMap
onValueChanged h = addHandler (ValueChangedHandler h) mempty
