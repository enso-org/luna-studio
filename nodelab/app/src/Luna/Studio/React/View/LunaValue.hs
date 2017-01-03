{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Luna.Studio.React.View.LunaValue where

import           Data.Aeson                      (ToJSON, toJSON)
import           React.Flux

import           Luna.Studio.Commands.Command    (Command)
import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude
import qualified Luna.Studio.State.Global        as Global
import           Luna.Studio.State.UIRegistry    (addHandler)
import           Object.UITypes                  (WidgetId)
import           UI.Handlers.Generic             (ValueChangedHandler (..), triggerValueChanged)


newtype LunaExpression = LunaExpression String

instance Show LunaExpression where
    show (LunaExpression s) = s

class (Eq a, Show a, ToJSON a) => LunaValue a where
    asLunaExpr           :: a -> LunaExpression
    createValueWidget'   :: a -> Text -> Double -> ReactElementM ViewEventHandler ()


data AnyLunaValue = forall a. (LunaValue a)=> AnyLunaValue {unAnyLunaValue :: a}

instance Eq AnyLunaValue where
    _a == _b = undefined

createValueWidget :: AnyLunaValue -> Text -> Double -> ReactElementM ViewEventHandler ()
createValueWidget (AnyLunaValue a) label width = createValueWidget' a label width

deriving instance Show AnyLunaValue
instance ToJSON AnyLunaValue where
    toJSON (AnyLunaValue a) = toJSON a




controlHandler :: LunaValue a => a -> WidgetId -> Command Global.State ()
controlHandler val = triggerValueChanged (AnyLunaValue val)

instance LunaValue Int    where
    asLunaExpr = LunaExpression . show
    createValueWidget' val label width = do
        div_ $ elemString "Int"
        --TODO[react] implement
        -- let widget    = DiscreteNumber.create (Vector2 width 20) label val
        --     handlers' = addHandler (ValueChangedHandler (controlHandler :: Int -> WidgetId -> Command Global.State ())) $ handlers
        -- UICmd.register parent widget handlers'

instance LunaValue Double where
    asLunaExpr = LunaExpression . show
    createValueWidget' val label width = do
        div_ $ elemString "Double"
        --TODO[react] implement
        -- let widget    = ContinuousNumber.create (Vector2 width 20) label val
        --     handlers' = addHandler (ValueChangedHandler (controlHandler :: Double -> WidgetId -> Command Global.State ())) $ handlers
        -- UICmd.register parent widget handlers'

instance LunaValue Bool   where
    asLunaExpr = LunaExpression . show
    createValueWidget' val label width = do
        div_ $ elemString "Bool"
        --TODO[react] implement
        -- let widget    = Toggle.create (Vector2 width 20) label val
        --     handlers' = addHandler (ValueChangedHandler (controlHandler :: Bool -> WidgetId -> Command Global.State ())) $ handlers
        -- UICmd.register parent widget handlers'
