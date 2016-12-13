{-# LANGUAGE OverloadedStrings #-}

module Object.LunaValue.Instances where

import           Object.UITypes                  (WidgetId)
import           Luna.Studio.Commands.Command       (Command)
import           Luna.Studio.Commands.UIRegistry    as UICmd
import qualified Luna.Studio.State.Global           as Global
import           Luna.Studio.State.UIRegistry       (addHandler)
import           Luna.Studio.Prelude               hiding (Choice)
import           Luna.Studio.Data.Vector

import           Object.LunaValue

import qualified Object.Widget.Number.Continuous as ContinuousNumber
import qualified Object.Widget.Number.Discrete   as DiscreteNumber
import qualified Object.Widget.Toggle            as Toggle
import           UI.Handlers.Generic             (ValueChangedHandler (..), triggerValueChanged)
import           UI.Instances                    ()

controlHandler :: LunaValue a => a -> WidgetId -> Command Global.State ()
controlHandler val = triggerValueChanged (AnyLunaValue val)

instance LunaValue Int    where
    asLunaExpr = LunaExpression . show
    createValueWidget' parent val label width handlers = do
        let widget    = DiscreteNumber.create (Vector2 width 20) label val
            handlers' = addHandler (ValueChangedHandler (controlHandler :: Int -> WidgetId -> Command Global.State ())) $ handlers
        UICmd.register parent widget handlers'

instance LunaValue Double where
    asLunaExpr = LunaExpression . show
    createValueWidget' parent val label width handlers = do
        let widget    = ContinuousNumber.create (Vector2 width 20) label val
            handlers' = addHandler (ValueChangedHandler (controlHandler :: Double -> WidgetId -> Command Global.State ())) $ handlers
        UICmd.register parent widget handlers'

instance LunaValue Bool   where
    asLunaExpr = LunaExpression . show
    createValueWidget' parent val label width handlers = do
        let widget    = Toggle.create (Vector2 width 20) label val
            handlers' = addHandler (ValueChangedHandler (controlHandler :: Bool -> WidgetId -> Command Global.State ())) $ handlers
        UICmd.register parent widget handlers'
