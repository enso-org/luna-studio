{-# LANGUAGE ExistentialQuantification #-}

module Object.LunaValue where

import           Luna.Studio.Prelude         hiding (Choice)
import qualified Luna.Studio.State.UIRegistry as UIRegistry
import           Object.UITypes            (WidgetId)
import           Luna.Studio.Commands.Command (Command)
import           Data.HMap.Lazy (HTMap)
import           Data.Aeson (ToJSON, toJSON)
newtype LunaExpression = LunaExpression String

instance Show LunaExpression where
    show (LunaExpression s) = s

class (Eq a, Show a, ToJSON a) => LunaValue a where
    asLunaExpr           :: a -> LunaExpression
    createValueWidget'   :: WidgetId -> a -> Text -> Double -> HTMap -> Command UIRegistry.State WidgetId


data AnyLunaValue = forall a. (LunaValue a)=> AnyLunaValue {unAnyLunaValue :: a}

instance Eq AnyLunaValue where
    _a == _b = undefined

createValueWidget :: WidgetId -> AnyLunaValue -> Text -> Double -> HTMap -> Command UIRegistry.State WidgetId
createValueWidget id (AnyLunaValue a) label width handlers = createValueWidget' id a label width handlers

deriving instance Show AnyLunaValue
instance ToJSON AnyLunaValue where
    toJSON (AnyLunaValue a) = toJSON a
