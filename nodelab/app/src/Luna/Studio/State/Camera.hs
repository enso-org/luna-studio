module Luna.Studio.State.Camera where

import           Luna.Studio.Commands.Command       (Command)
import           Luna.Studio.Data.Vector            (Position, move, rescale)
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Model.NodeEditor as NodeEditor
import qualified Luna.Studio.React.Store            as Store
import           Luna.Studio.State.Global           (State)
import qualified Luna.Studio.State.Global           as Global


-- TODO[react]: Implement this correctly
translateToWorkspace :: Position -> Command State (Position)
translateToWorkspace pos = do
    factor <- Global.withNodeEditor $ Store.use NodeEditor.factor
    pan <- Global.withNodeEditor $ Store.use NodeEditor.pan
    return $ rescale (move pos $ -pan) $ 1/factor

-- TODO[react]: remove once camera is reimplemented
-- data DragHistory =  PanDragHistory  { _panPreviousPos :: Position }
--                  | ZoomDragHistory  { _zoomPreviousPos :: Position
--                                     , _zoomFPScreen    :: Vector2 Int
--                                     , _zoomFPWorkspace :: Vector2 Double }
--                  deriving (Eq, Show, Generic)
--
-- data Camera = Camera { _screenSize :: Vector2 Int
--                      , _windowSize :: Vector2 Int
--                      , _pan        :: Vector2 Double
--                      , _factor     :: Double
--                      } deriving (Eq, Show, Generic)
--
-- data State = State { _camera  :: Camera
--                    , _history :: Maybe DragHistory
--                    } deriving (Eq, Show, Generic)
--
-- makeLenses ''State
-- makeLenses ''Camera
-- makeLenses ''DragHistory
--
-- instance ToJSON State
-- instance ToJSON Camera
-- instance ToJSON DragHistory
--
-- instance Default Camera where
--     def = Camera (Vector2 400 200) (Vector2 400 200) def 1.0
--
-- instance Default State where
--     def = State def def
--
-- glToWorkspace :: Camera -> Position -> Position
-- glToWorkspace (Camera _ _ pan factor) (Vector2 xGl yGl) = Vector2
--     (xGl / factor + pan ^. x)
--     (yGl / factor + pan ^. y)
--
-- screenToGl :: Vector2 Int -> Position -> Position
-- screenToGl (Vector2 screenSizeX screenSizeY) (Vector2 x y) = Vector2
--     (fromIntegral x - (fromIntegral screenSizeX) / 2.0)
--     (fromIntegral y - (fromIntegral screenSizeY) / 2.0)
--
-- scaledScreenToWorkspace :: Double -> Vector2 Int -> Vector2 Double
-- scaledScreenToWorkspace factor delta =
--     (/ factor) . fromIntegral <$> delta
--
-- scaledScreenToWorkspaceM :: Vector2 Int -> Command State (Vector2 Double)
-- scaledScreenToWorkspaceM delta = do
--     factor <- use $ camera . factor
--     return $ (/ factor) . fromIntegral <$> delta
--
-- screenToWorkspace :: Camera -> Position -> Position
-- screenToWorkspace camera pos =
--     glToWorkspace camera $ screenToGl (camera ^. screenSize) pos
--
-- screenToWorkspaceM :: Position -> Command State (Position)
-- screenToWorkspaceM pos = do
--     camera <- use camera
--     return $ screenToWorkspace camera pos
--
-- workspaceToScreen :: Position -> Command State (Position)
-- workspaceToScreen (Vector2 px py) = do
--     screenSize' <- use $ camera . screenSize
--     pan'        <- use $ camera . pan
--     factor'     <- use $ camera . factor
--     let px' = floor $ (px - pan' ^. x) * factor'  + (fromIntegral $ screenSize' ^. x) / 2.0
--         py' = floor $ (py - pan' ^. y) * factor'  + (fromIntegral $ screenSize' ^. y) / 2.0
--     return $ Vector2 px' py'
