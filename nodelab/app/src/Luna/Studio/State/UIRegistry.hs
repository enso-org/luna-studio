{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Studio.State.UIRegistry
    ( Object.Widget.State
    , Object.Widget.WidgetMap
    , dragState
    , widgets
    , registerM
    , registerIxM
    , lookupTypedM
    , updateWidgetM
    , lookupM
    , widgetOver
    , unregisterM
    , focusedWidget
    , addHandler
    , sceneGraphId
    , sceneInterfaceId
    , lookupAllM
    , mouseDownWidget
    , lookupAll
    , currentConnectionId
    , handle
    , LookupFor
    ) where

import           Control.Arrow                      (first)
import qualified Control.Monad.State                as MState
import           Control.Monad.Trans.RWS            (RWS)
import qualified Control.Monad.Trans.RWS            as RWS
import           Data.Aeson                         (ToJSON, object, toJSON)
import           Data.HMap.Lazy                     (HTMap)
import qualified Data.HMap.Lazy                     as HMap
import qualified Data.IntMap.Lazy                   as IntMap
import           Luna.Studio.Data.CtxDynamic
import           Luna.Studio.Prelude                hiding (children, lookup)

import           Luna.Studio.Action.Command         (Command)
import           Luna.Studio.Data.Aeson             (intMapToJSON)
import           Luna.Studio.React.Model.Connection
import           Object.Widget
import           Object.Widget.Scene


instance CompositeWidget Scene where
    createWidget _   _ = return ()
    updateWidget _ _ _ = return ()

instance ResizableWidget Scene

sceneInterfaceId, sceneGraphId, currentConnectionId :: WidgetId
sceneInterfaceId    = WidgetId 1
sceneGraphId        = WidgetId 2
currentConnectionId = WidgetId 3

makeLenses ''State

instance ToJSON State where
    toJSON st = object [ ("_widgets"        , intMapToJSON $ st ^. widgets  )
                       , ("_widgetOver"     , toJSON $ st ^. widgetOver     )
                       , ("_dragState"      , toJSON $ st ^. dragState      )
                       , ("_focusedWidget"  , toJSON $ st ^. focusedWidget  )
                       , ("_mouseDownWidget", toJSON $ st ^. mouseDownWidget)
                       ]

instance Show State where
    show a = show $ IntMap.size $ a ^. widgets

defaultWidgets :: [(WidgetId, WidgetFile DisplayObject)]
defaultWidgets = [ (sceneInterfaceId,     sceneInterface)
                 , (sceneGraphId,         sceneGraph)
                 ] where
    sceneInterface    = WidgetFile sceneInterfaceId    (toCtxDynamic $ Scene) Nothing [] def
    sceneGraph        = WidgetFile sceneGraphId        (toCtxDynamic $ Scene) Nothing [] def

instance Default State where
    def = State (fromList $ map (first fromWidgetId) defaultWidgets) def def def def

lookup :: WidgetId -> State -> Maybe (WidgetFile DisplayObject)
lookup widgetId state = IntMap.lookup (fromWidgetId widgetId) (state ^. widgets)

lookupM :: WidgetId -> Command State (Maybe (WidgetFile DisplayObject))
lookupM widgetId = preuse $ widgets . ix (fromWidgetId widgetId)

register' :: DisplayObjectClass a => ([WidgetId] -> WidgetId -> [WidgetId]) -> WidgetId -> a -> HTMap -> State -> (WidgetFile a, State)
register' childrenInsertFn parent a handlers state = (widgetFile, state & widgets .~ newWidgets') where
    newWidgets'   = IntMap.insert (fromWidgetId parent) newParent   newWidgets
    newWidgets    = IntMap.insert (fromWidgetId newId)  dynamicFile oldWidgets
    newId         = generateId state
    oldWidgets    = state ^. widgets
    (Just oldParent) = IntMap.lookup (fromWidgetId parent) oldWidgets
    newParent     = oldParent & children .~ childrenInsertFn (oldParent ^. children) newId
    dynamicFile   = WidgetFile newId (toCtxDynamic a) (Just parent) [] handlers
    widgetFile    = WidgetFile newId a (Just parent) [] handlers

register :: DisplayObjectClass a => WidgetId -> a -> HTMap -> State -> (WidgetFile a, State)
register = register' (\l n -> l ++ [n])

registerM :: DisplayObjectClass a => WidgetId -> a -> HTMap -> Command State (WidgetFile a)
registerM = MState.state .:. register

registerIx :: DisplayObjectClass a => Int -> WidgetId -> a -> HTMap -> State -> (WidgetFile a, State)
registerIx ix = register' newChildren where
    newChildren xs new_element = let (ys,zs) = splitAt ix xs   in ys ++ [new_element] ++ zs

registerIxM :: DisplayObjectClass a => Int -> WidgetId -> a -> HTMap -> Command State (WidgetFile a)
registerIxM = MState.state .:: registerIx

updateWidgetM :: DisplayObjectClass a => WidgetId -> (a -> a) -> Command State a
updateWidgetM widgetId fun = do
    maybeFile   <- lookupTypedM widgetId
    let file     = fromMaybe (error "updateWidgetM: invalidType") maybeFile
        newWidget  = fun $ file ^. widget

    widgets . ix (fromWidgetId widgetId) . widget .= toCtxDynamic newWidget

    return newWidget

unregisterRWS :: WidgetId -> RWS () [WidgetId] State ()
unregisterRWS oid = do
    widget <- RWS.gets $ lookup oid
    case widget of
        Just file -> sequence_ $ unregisterRWS <$> (file ^. children)
        Nothing   -> return ()
    RWS.modify (widgets %~ IntMap.delete (fromWidgetId oid))
    RWS.tell [oid]

unregister :: WidgetId -> State -> ([WidgetId], State)
unregister oid oldState = (outWidgets, state) where
    widgetParent        = oldState ^? widgets . (ix $ fromWidgetId oid) . parent
    (outWidgets, state) = case widgetParent of
        Just widgetParent -> (outWidgets, state') where
            (state, outWidgets) = RWS.execRWS (unregisterRWS oid) () oldState
            state' = case widgetParent of
                Just widgetParent -> state & widgets . ix (fromWidgetId widgetParent) . children %~ delete oid
                Nothing           -> state
        Nothing     -> ([], state)

unregisterM :: WidgetId -> Command State [WidgetId]
unregisterM = MState.state . unregister

generateId :: State -> WidgetId
generateId state = WidgetId $ if IntMap.size (state ^. widgets) == 0
    then 1
    else maxId + 1 where (maxId, _) = IntMap.findMax (state ^. widgets)

lookupAll :: DisplayObjectClass a => State -> [WidgetFile a]
lookupAll state = foldl process mempty objects where
    process acc obj = case fromCtxDynamic $ obj ^. widget of
        Just model -> (obj & widget .~ model):acc
        Nothing    -> acc
    objects = IntMap.elems $ state ^. widgets

lookupAllM :: DisplayObjectClass a => Command State [WidgetFile a]
lookupAllM = MState.gets lookupAll

lookupTyped :: DisplayObjectClass a => WidgetId -> State -> Maybe (WidgetFile a)
lookupTyped idx state = do
    object  <- lookup idx state
    model   <- fromCtxDynamic $ object ^. widget
    return $ object & widget .~ model

lookupTypedM :: DisplayObjectClass a => WidgetId -> Command State (Maybe (WidgetFile a))
lookupTypedM ix = MState.gets $ lookupTyped ix

type LookupFor a = Command State (Maybe (WidgetFile a))

addHandler :: Typeable v => v -> HTMap -> HTMap
addHandler h m = HMap.insert (undefined :: HMap.TypeKey v) h m

handle :: Typeable v => v -> HTMap
handle h = HMap.insert (undefined :: HMap.TypeKey v) h mempty
