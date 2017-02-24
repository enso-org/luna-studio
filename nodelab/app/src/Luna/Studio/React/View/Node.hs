{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Node where


import qualified Data.Aeson                            as Aeson
import           Data.Matrix                           as Matrix
import           Data.Matrix                           (Matrix)
import           Data.Position                         (Position(Position),Vector2(Vector2),x, y)
import qualified Data.Text                             as Text
import           Empire.API.Data.Node                  (NodeId)
import qualified JS.Config                             as Config
import           Luna.Studio.Action.Geometry.Constants (fontSize)
import qualified Luna.Studio.Event.Mouse               as Mouse
import qualified Luna.Studio.Event.UI                  as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Node          as Node
import           Luna.Studio.React.Model.App           (App)
import           Luna.Studio.React.Model.Node          (Node)
import qualified Luna.Studio.React.Model.Node          as Node
import           Luna.Studio.React.Store               (Ref, dispatch)
import           Luna.Studio.React.View.NodeBody       (nodeBody_)
import           Luna.Studio.React.View.Style          (lunaPrefix)
import           React.Flux
import qualified React.Flux                            as React


objName :: JSString
objName = "node"

nodePrefix :: JSString
nodePrefix = Config.prefix "node-"

stylePrefix :: JSString -> JSString
stylePrefix = (<>) $ lunaPrefix objName <> "-"

handleMouseDown :: Ref App -> NodeId -> Event -> MouseEvent -> [SomeStoreAction]
handleMouseDown ref nodeId e m =
    if Mouse.withoutMods m Mouse.leftButton
    then stopPropagation e : dispatch ref (UI.NodeEvent $ Node.MouseDown m nodeId)
    else []

node :: ReactView (Ref App, Node)
node = React.defineView objName $ \(ref, n) -> do
    let nodeId    = n ^. Node.nodeId
        nodeLimit = 10000::Int
        zIndex    = n ^. Node.zPos
        z         = if n ^. Node.isExpanded then zIndex + nodeLimit else zIndex
    div_
        [ "key"       $= (nodePrefix <> fromString (show nodeId))
        , "id"        $= (nodePrefix <> fromString (show nodeId))
        , "className" $= stylePrefix "root"
        , "style"     @= Aeson.object [ "zIndex" Aeson..= show z ]
        ] $ do
        div_
            [ "key"       $= "nodeTrans"
            , "className" $= stylePrefix "trans"
            ] $
            nodeBody_ ref n
        div_
            [ "key"       $= "nameTrans"
            , "className" $= "luna-name-trans"
            ] $
            div_
                [ "key"         $= "nameRoot"
                , onClick       $ \_ m -> dispatch ref $ UI.NodeEvent $ Node.Select m nodeId
                , onDoubleClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.Enter nodeId
                , onMouseDown   $ handleMouseDown ref nodeId
                , "className"   $= (lunaPrefix objName <> if n ^. Node.isExpanded then " " <> lunaPrefix objName <> "--expanded" else " " <> lunaPrefix objName <> "--collapsed"
                                                       <> if n ^. Node.isSelected then " " <> lunaPrefix objName <> "--selected" else "")
                ] $
                svg_
                    [ "key" $= "name" ] $
                    text_
                        [ "key"         $= "nameText"
                        , onDoubleClick $ \e _ -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.EditExpression nodeId)
<<<<<<< HEAD
                        , "className"   $= (lunaPrefix objName <> "__name " <> stylePrefix "noselect")
                        ] $ elemString $ Text.unpack $ n ^. Node.expression
=======
                        , "className"   $= "luna-node__name luna-noselect"
                        ] $ elemString $ convert $ n ^. Node.expression
>>>>>>> d89c1adb6fb803c48f9fb64a3d85dd7143920126

node_ :: Ref App -> Node -> ReactElementM ViewEventHandler ()
node_ ref model = React.viewWithSKey node (jsShow $ model ^. Node.nodeId) (ref, model) mempty

nodeDynamicStyles_ :: Matrix Double -> Node -> ReactElementM ViewEventHandler ()
nodeDynamicStyles_ camera n = do
    let nodeId = n ^. Node.nodeId
        pos    = expressionPosition camera (n ^. Node.position)
        scale  = (Matrix.toList camera)!!0
    elemString $ "#" <> Config.mountPoint <> "-node-" <> fromString (show nodeId)
                     <> " .luna-name-trans { transform: translate(" <> show (pos ^. x) <> "px, " <> show (pos ^. y) <> "px)"
                     <> if scale > 1 then "; font-size: " <> show (fontSize * scale) <> "px }" else " }"

expressionPosition :: Matrix Double -> Position -> Position
expressionPosition camera n = Position (Vector2 x' y')
    where posX  = n ^. x
          posY  = n ^. y - 36
          camX  = (Matrix.toList camera)!!12
          camY  = (Matrix.toList camera)!!13
          scale = (Matrix.toList camera)!!0
          x'    = fromInteger (round $ camX + (scale * posX) :: Integer)
          y'    = fromInteger (round $ camY + (scale * posY) :: Integer)
