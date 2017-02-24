{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Node where

import qualified Data.Aeson                      as Aeson
import           Data.Matrix                     as Matrix
import           Data.Matrix                     (Matrix)
import           Data.Position                   (x, y)
import           Empire.API.Data.Node            (NodeId)
import qualified JS.Config                       as Config
import qualified Luna.Studio.Event.Mouse         as Mouse
import qualified Luna.Studio.Event.UI            as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Node    as Node
import           Luna.Studio.React.Model.App     (App)
import           Luna.Studio.React.Model.Node    (Node)
import qualified Luna.Studio.React.Model.Node    as Node
import           Luna.Studio.React.Store         (Ref, dispatch)
import           Luna.Studio.React.View.NodeBody (nodeBody_)
import           React.Flux
import qualified React.Flux                      as React


objName :: JSString
objName = "node"

nodePrefix :: JSString
nodePrefix = Config.prefix "node-"

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
        , "className" $= "luna-node-root"
        , "style"     @= Aeson.object [ "zIndex" Aeson..= show z ]
        ] $ do
        div_
            [ "key"       $= "nodeTrans"
            , "className" $= "luna-node-trans"
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
                , "className"   $= (fromString $ "luna-node" <> if n ^. Node.isExpanded then " luna-node--expanded" else " luna-node--collapsed"
                                                             <> if n ^. Node.isSelected then " luna-node--selected" else [])
                ] $
                svg_
                    [ "key" $= "name" ] $
                    text_
                        [ "key"         $= "nameText"
                        , onDoubleClick $ \e _ -> stopPropagation e : dispatch ref (UI.NodeEvent $ Node.EditExpression nodeId)
                        , "className"   $= "luna-node__name luna-noselect"
                        ] $ elemString $ convert $ n ^. Node.expression

node_ :: Ref App -> Node -> ReactElementM ViewEventHandler ()
node_ ref model = React.viewWithSKey node (jsShow $ model ^. Node.nodeId) (ref, model) mempty

nodeDynamicStyles_ :: Matrix Double -> Node -> ReactElementM ViewEventHandler ()
nodeDynamicStyles_ camera n = do
    let nodeId = show $ n ^. Node.nodeId
        posX   = n ^. Node.position ^. x
        posY   = n ^. Node.position ^. y - 36
        camX   = (Matrix.toList camera)!!12
        camY   = (Matrix.toList camera)!!13
        scale  = (Matrix.toList camera)!!0
        nx     = show (round $ camX + (scale * posX) :: Integer)
        ny     = show (round $ camY + (scale * posY) :: Integer)
    elemString $ "#" <> Config.mountPoint <> "-node-" <> fromString nodeId <> " .luna-name-trans { transform: translate(" <> nx <> "px, " <> ny <> "px)"
                     <> if scale > 1 then "; font-size: " <> show (12 * scale) <> "px }" else " }"
