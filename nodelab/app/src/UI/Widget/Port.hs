{-# LANGUAGE OverloadedStrings #-}

module UI.Widget.Port where

import           Utils.PreludePlus

import           GHCJS.Marshal.Pure           (PFromJSVal (..), PToJSVal (..))

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Port           as Model

import           Reactive.Commands.Command    (Command)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.UIRegistry    as UIRegistry (State)

import qualified UI.Handlers.Node             as Node
import qualified UI.Registry                  as UIR
import           UI.Widget                    (UIWidget)
import           UI.Widget                    (GenericWidget (..))
import qualified UI.Widget                    as Widget

import           Empire.API.Data.Port         (InPort (Self))
import           Empire.API.Data.PortRef      (AnyPortRef (InPortRef'), InPortRef (..))

newtype Port = Port { unPort :: JSVal } deriving (PToJSVal, PFromJSVal)

instance UIWidget Port

foreign import javascript safe "new Port($1, $2)"        create'      :: WidgetId -> Bool   -> IO Port
foreign import javascript safe "$1.setAngle($2, $3, $4)" setAngle     :: Port     -> Double -> Int -> Bool -> IO ()
foreign import javascript safe "$1.setColor($2)"         setColor     :: Port     -> Int    -> IO ()
foreign import javascript safe "$1.setHighlight($2)"     setHighlight :: Port     -> Bool   -> IO ()


isSelf :: AnyPortRef -> Bool
isSelf (InPortRef' (InPortRef _ Self)) = True
isSelf _ = False

create :: WidgetId -> Model.Port -> IO Port
create wid model = do
    port <- create' wid (isSelf $ model ^. Model.portRef)
    setAngle     port  (model ^. Model.angle) (model ^. Model.portCount) (model ^. Model.isOnly)
    setColor     port $ model ^. Model.color
    setHighlight port $ model ^. Model.highlight
    return port

instance UIDisplayObject Model.Port where
    createUI parentId wid model = do
        widget  <- create wid model
        parent  <- UIR.lookup parentId :: IO GenericWidget
        UIR.register wid widget
        Widget.add widget  parent

    updateUI wid _old model = do
        port <- UIR.lookup wid :: IO Port
        setAngle     port  (model ^. Model.angle) (model ^. Model.portCount) (model ^. Model.isOnly)
        setColor     port $ model ^. Model.color
        setHighlight port $ model ^. Model.highlight

toNodeId :: WidgetId -> Command UIRegistry.State WidgetId
toNodeId wid = UICmd.parent wid >>= UICmd.parent

onMouseOver, onMouseOut :: WidgetId -> Command Global.State ()
onMouseOver wid = inRegistry $ do
    UICmd.update_ wid $ Model.highlight .~ True
    nodeId <- toNodeId wid
    Node.showHidePortLabels True nodeId
onMouseOut  wid = inRegistry $ do
    UICmd.update_ wid $ Model.highlight .~ False
    nodeId <- toNodeId wid
    Node.showHidePortLabels False nodeId

-- selectNode evt _ wid = do --TODO[react]
--     nodeId <- inRegistry $ toNodeId wid
--     Node.selectNode evt nodeId

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & mouseOver .~ const onMouseOver
                     & mouseOut  .~ const onMouseOut
                    --  & mousePressed .~ selectNode --TODO[react]

instance CompositeWidget Model.Port
instance ResizableWidget Model.Port
