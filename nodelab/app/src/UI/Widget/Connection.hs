{-# LANGUAGE OverloadedStrings #-}

module UI.Widget.Connection where

import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude

import           GHCJS.Marshal.Pure       (PFromJSVal (..), PToJSVal (..))

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Connection as Model

import qualified UI.Registry              as UIR
import           UI.Widget                (UIWidget)
import           UI.Widget                (GenericWidget (..))
import qualified UI.Widget                as UIT

newtype Connection = Connection { unConnection :: JSVal } deriving (PToJSVal, PFromJSVal)

instance UIWidget Connection

foreign import javascript safe "new Connection($1)"            create'       :: Int   -> IO Connection
foreign import javascript safe "$1.setPos($2, $3, $4, $5, $6)" setPos        :: Connection -> Double -> Double -> Double -> Double -> Int -> IO ()
foreign import javascript safe "$1.setVisible($2)"             setVisible    :: Connection -> Bool    -> IO ()
foreign import javascript safe "common.commonUniforms.isConnecting.value = ($1?1:0)" setIsConnecting :: Bool    -> IO ()
foreign import javascript safe "$1.setColor($2)"               setColor      :: Connection -> Int     -> IO ()
foreign import javascript safe "$1.setArrow($2)"               setArrow      :: Connection -> Bool    -> IO ()
foreign import javascript safe "$1.setHighlight($2)"           setHighlight' :: Connection -> Int     -> IO ()


setHighlight :: Connection -> Model.ConnectionHighlight -> IO ()
setHighlight conn hl = setHighlight' conn $ case hl of
    Model.None         -> 0
    Model.SrcHighlight -> 1
    Model.DstHighlight -> 2

create :: WidgetId -> Model.Connection -> IO Connection
create (WidgetId widgetId) model = do
    connection <- create' widgetId
    setPos     connection (model ^. Model.from . x)  (model ^. Model.from . y)  (model ^. Model.to . x)  (model ^. Model.to . y) widgetId
    setVisible connection (model ^. Model.visible)
    setArrow   connection (model ^. Model.arrow)
    --TODO[react] : remove
    -- setColor   connection (model ^. Model.color)
    setHighlight connection (model ^. Model.highlight)
    return connection

instance UIDisplayObject Model.Connection where
    createUI parentId widgetId model = do
        widget  <- create widgetId model
        parent  <- UIR.lookup parentId :: IO GenericWidget
        UIR.register widgetId widget
        UIT.add widget parent

    updateUI widgetId old model = do
        connection <- UIR.lookup widgetId :: IO Connection
        setPos     connection (model ^. Model.from . x)  (model ^. Model.from . y)  (model ^. Model.to . x)  (model ^. Model.to . y) (fromWidgetId widgetId)
        setVisible connection (model ^. Model.visible)
        setArrow   connection (model ^. Model.arrow)
        --TODO[react] remove
        -- setColor   connection (model ^. Model.color)
        setHighlight connection (model ^. Model.highlight)

instance UIDisplayObject Model.CurrentConnection where
    createUI = undefined
    updateUI widgetId _ model = do
        connection <- UIR.lookup widgetId :: IO Connection
        setPos     connection (model ^. Model.currentFrom . x)  (model ^. Model.currentFrom . y)  (model ^. Model.currentTo . x)  (model ^. Model.currentTo . y) (fromWidgetId widgetId)
        setVisible connection (model ^. Model.currentVisible)
        setArrow   connection (model ^. Model.currentArrow)
        --TODO[react] remove
        -- setColor   connection (model ^. Model.currentColor)
        setIsConnecting (model ^. Model.currentVisible)

instance CompositeWidget Model.Connection
instance ResizableWidget Model.Connection

instance CompositeWidget Model.CurrentConnection
instance ResizableWidget Model.CurrentConnection
