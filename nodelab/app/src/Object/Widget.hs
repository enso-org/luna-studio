{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Object.Widget (
    module Object.Widget,
    module Object.UITypes
) where

import           Data.Aeson                   (ToJSON, object, toJSON, (.=))
import           Data.HMap.Lazy               (HTMap)
import           Data.IntMap.Lazy             (IntMap)
import           Event.Event                  (JSState)
import           Event.Keyboard               (KeyMods)
import qualified Event.Keyboard               as Keyboard
import           Event.Mouse                  (MouseButton)
import qualified Event.Mouse                  as Mouse
import           Event.Widget                 (Payload)
import           Luna.Studio.Commands.Command (Command)
import           Object.UITypes
-- import           Luna.Studio.State.Camera     (Camera)
-- import qualified Luna.Studio.State.Camera     as Camera
import           Luna.Studio.Data.CtxDynamic
import           Luna.Studio.Data.Vector      (Position, Size)
import           Luna.Studio.Prelude          hiding (children, (.=))


type DisplayObject = CtxDynamic DisplayObjectClass

type DisplayObjectCtx a =   ( Show a
                            , Typeable a
                            , IsDisplayObject a
                            , UIDisplayObject a
                            , ResizableWidget a
                            , ToJSON a
                            )

class    DisplayObjectCtx a => DisplayObjectClass a
instance DisplayObjectCtx a => DisplayObjectClass a

-- class DisplayObjectClass a => DisplayObjectContainer a


data WidgetFile b = WidgetFile { _objectId :: WidgetId
                               , _widget   :: b
                               , _parent   :: Maybe WidgetId
                               , _children :: [WidgetId]
                               , _handlers :: HTMap
                               } deriving (Generic)

class IsDisplayObject a where
    widgetPosition :: Lens'  a Position
    widgetSize     :: Lens'  a Size
    widgetVisible  :: Getter a Bool

class UIDisplayObject a where
    createUI   :: WidgetId -> WidgetId       -> a -> IO ()
    updateUI   :: WidgetId -> a              -> a -> IO ()

getPosition :: DisplayObject -> Position
getPosition obj = withCtxDynamic (^. widgetPosition) obj

getSize :: DisplayObject -> Size
getSize obj = withCtxDynamic (^. widgetSize) obj

getVisible :: DisplayObject -> Bool
getVisible obj = withCtxDynamic (^. widgetVisible) obj

setPosition' :: DisplayObjectClass a => Position -> a -> DisplayObject
setPosition' pos obj = toCtxDynamic $ obj & widgetPosition .~ pos

setPosition :: DisplayObject -> Position -> DisplayObject
setPosition obj pos = withCtxDynamic (setPosition' pos) obj

setSize' :: DisplayObjectClass a => Size -> a -> DisplayObject
setSize' size obj = toCtxDynamic $ obj & widgetSize .~ size

setSize :: DisplayObject -> Size -> DisplayObject
setSize obj size = withCtxDynamic (setSize' size) obj

instance IsDisplayObject DisplayObject where
    widgetPosition = lens getPosition setPosition
    widgetSize     = lens getSize     setSize
    widgetVisible  = to getVisible

instance Show DisplayObject where
    show = withCtxDynamic show

instance ToJSON DisplayObject where
    toJSON = withCtxDynamic toJSON

class CompositeWidget a where
    createWidget :: WidgetId -> a      -> Command State ()
    updateWidget :: WidgetId -> a -> a -> Command State ()
    default createWidget :: WidgetId -> a -> Command State ()
    createWidget _ _   = return ()
    default updateWidget :: WidgetId -> a -> a -> Command State()
    updateWidget _ _ _ = return ()

class ResizableWidget a where
    resizeWidget    :: WidgetId -> Size -> a -> Command State ()
    default resizeWidget :: WidgetId -> Size -> a -> Command State ()
    resizeWidget _ _ _ = return ()

instance ResizableWidget DisplayObject where
    resizeWidget wid size obj = withCtxDynamic (resizeWidget wid size) obj

data DragState = DragState { _widgetId     :: WidgetId
                           , _widgetMatrix :: [Double]
                           , _scene        :: SceneType
                           , _button       :: MouseButton
                           , _keyMods      :: Keyboard.KeyMods
                           , _startPos     :: Position
                           , _previousPos  :: Position
                           , _currentPos   :: Position
                           } deriving (Show, Eq, Generic)

type WidgetMap = IntMap (WidgetFile DisplayObject)

data State = State { _widgets         :: WidgetMap
                   , _widgetOver      :: Maybe WidgetId
                   , _dragState       :: Maybe DragState
                   , _focusedWidget   :: Maybe WidgetId
                   , _mouseDownWidget :: Maybe WidgetId
                   } deriving (Generic)


instance ToJSON DragState

--TODO[react]: Why this is even here???
-- TODO: fix non-exhaustive pattern
-- sceneToLocal :: Vector2 Double -> [Double] -> Vector2 Double
-- sceneToLocal (Vector2 x y) [ aa, ab, _ , _
--                            , ba, bb, _ , _
--                            , _ , _ , _ , _
--                            , da, db, _ , _
--                            ] = Vector2 x' y' where
--                                x' = aa * x + ba * y + da
--                                y' = ab * x + bb * y + db

--TODO[react]: Why this is even here???
-- screenToLocal :: Camera -> Position -> [Double]  -> Position
-- screenToLocal cam mousePos widgetMatrix = sceneToLocal workspacePos widgetMatrix where
--     workspacePos = Camera.screenToWorkspace cam mousePos

type MouseMoveHandler     s =        Mouse.Event' -> JSState -> WidgetId -> Command s ()
type MousePressedHandler  s =        Mouse.Event' -> JSState -> WidgetId -> Command s ()
type MouseReleasedHandler s =        Mouse.Event' -> JSState -> WidgetId -> Command s ()
type MouseOverHandler     s =                        JSState -> WidgetId -> Command s ()
type MouseOutHandler      s =                        JSState -> WidgetId -> Command s ()
type ClickHandler         s =        Mouse.Event' -> JSState -> WidgetId -> Command s ()
type DblClickHandler      s =        Mouse.Event' -> JSState -> WidgetId -> Command s ()
type KeyUpHandler         s = Char -> KeyMods     -> JSState -> WidgetId -> Command s ()
type KeyDownHandler       s = Char -> KeyMods     -> JSState -> WidgetId -> Command s ()
type KeyPressedHandler    s = Char -> KeyMods     -> JSState -> WidgetId -> Command s ()
type DragMoveHandler      s =         DragState   -> JSState -> WidgetId -> Command s ()
type DragEndHandler       s =         DragState   -> JSState -> WidgetId -> Command s ()
type WidgetCustomHandler  s =                        Payload -> WidgetId -> Command s ()

data UIHandlers a  = UIHandlers { _mouseMove     :: MouseMoveHandler      a
                                , _mousePressed  :: MousePressedHandler   a
                                , _mouseReleased :: MouseReleasedHandler  a
                                , _mouseOver     :: MouseOverHandler      a
                                , _mouseOut      :: MouseOutHandler       a
                                , _click         :: ClickHandler          a
                                , _dblClick      :: DblClickHandler       a
                                , _keyUp         :: KeyUpHandler          a
                                , _keyDown       :: KeyDownHandler        a
                                , _keyPressed    :: KeyPressedHandler     a
                                , _dragMove      :: DragMoveHandler       a
                                , _dragEnd       :: DragEndHandler        a
                                , _widgetCustom  :: WidgetCustomHandler   a
                                }

instance Default (UIHandlers a) where
    def = UIHandlers (\     _ _ _ -> return ())
                     (\     _ _ _ -> return ())
                     (\     _ _ _ -> return ())
                     (\       _ _ -> return ())
                     (\       _ _ -> return ())
                     (\     _ _ _ -> return ())
                     (\     _ _ _ -> return ())
                     (\ _   _ _ _ -> return ())
                     (\ _   _ _ _ -> return ())
                     (\ _   _ _ _ -> return ())
                     (\     _ _ _ -> return ())
                     (\     _ _ _ -> return ())
                     (\       _ _ -> return ())


makeLenses ''DragState
makeLenses ''UIHandlers
makeLenses ''WidgetFile

widgetType :: DisplayObject -> String
widgetType (CtxDynamic tpe _) = show tpe

instance ToJSON (WidgetFile DisplayObject) where
    toJSON file = object [ "_objectId" .= (toJSON $ file ^. objectId)
                         , "_widget"   .= (toJSON $ file ^. widget  )
                         , "_parent"   .= (toJSON $ file ^. parent  )
                         , "_children" .= (toJSON $ file ^. children)
                         , "_type"     .= (toJSON $ widgetType $ file ^. widget)
                         ]
