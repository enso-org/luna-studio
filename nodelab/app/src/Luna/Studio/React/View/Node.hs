{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Node where

import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector             (x, y)
import qualified Data.Map.Lazy            as Map
import qualified Data.Text.Lazy           as Text
import           React.Flux
import qualified React.Flux               as React

import qualified Event.UI                 as UI
import           Empire.API.Data.Port     (InPort (..), PortId (..))
import           Object.Widget.Port       (Port (..))
import           Luna.Studio.React.Store              (Ref, dispatch, dt)
import           Luna.Studio.React.Model.Node         (Node)
import qualified Luna.Studio.React.Model.Node         as Node
import qualified Luna.Studio.React.Event.Node         as Node
import           Luna.Studio.React.View.Port          (port_)
import           Luna.Studio.React.View.Visualization (strValue, visualization_)



name :: JSString
name = "node-editor"

isIn :: Port -> Int
isIn (Port _ (InPortId (Arg _)) _ _) = 1
isIn _ = 0

isOut :: Port -> Int
isOut (Port _ (OutPortId _) _ _) = 1
isOut _ = 0

countPorts :: Port -> [Port] -> Int
countPorts (Port _ (InPortId _)  _ _) ports = foldl (\acc p -> acc + (isIn p))  0 ports
countPorts (Port _ (OutPortId _) _ _) ports = foldl (\acc p -> acc + (isOut p)) 0 ports

--FIXME: move all styles to CSS
node :: Ref Node -> ReactView ()
node nodeRef = React.defineControllerView
    name nodeRef $ \nodeStore () -> do
        let n         = nodeStore ^. dt
            nodeId    = n ^. Node.nodeId
            pos       = n ^. Node.position
            ports     = Map.elems $ n ^. Node.ports
            translate = fromString $ "translate(" <> show (pos ^. x) <> "," <> show (pos ^. y) <> ")" -- TODO: Consider implementing matrices
        if n ^. Node.isExpanded then
             g_
                 [ onClick       $ \_ m -> dispatch nodeRef $ UI.NodeEvent $ Node.Select m nodeId
                 , onDoubleClick $ \_ _ -> dispatch nodeRef $ UI.NodeEvent $ Node.Enter nodeId
                 , onMouseDown   $ \e m -> stopPropagation e : dispatch nodeRef (UI.NodeEvent $ Node.MouseDown m nodeId)
                 , "className" $= (fromString $ "node node--collapsed" <> (if n ^. Node.isSelected then " node--selected" else []))
                 , "transform" $= translate
                 , "key"       $= fromString (show nodeId)
                 ] $ do
                     circle_
                         [ "className" $= "selection-mark"
                         ] mempty

                     forM_ ports $ \port -> port_ nodeRef port (countPorts port ports)

                     text_
                         [ "className" $= "name"
                         , "x"         $= "20"
                         , "y"         $= "-16"
                         ] $ elemString $ Text.unpack (n ^. Node.expression) <> " EXPANDED"
                     text_
                         [ "className" $= "name"
                         , "x"         $= "20"
                         , "y"         $= "65"
                         ] $ elemString $ strValue n
                     g_  [ "transform" $= "translate(-25,80)"
                         ] $ forM_ (n ^. Node.value) visualization_
        else
            g_
                [ onClick       $ \_ m -> dispatch nodeRef $ UI.NodeEvent $ Node.Select m nodeId
                , onDoubleClick $ \_ _ -> dispatch nodeRef $ UI.NodeEvent $ Node.Enter nodeId
                , onMouseDown   $ \e m -> stopPropagation e : dispatch nodeRef (UI.NodeEvent $ Node.MouseDown m nodeId)
                , "className" $= (fromString $ "node node--collapsed" <> (if n ^. Node.isSelected then " node--selected" else []))
                , "transform" $= translate
                , "key"       $= fromString (show nodeId)
                ] $ do
                    circle_
                        [ "className" $= "selection-mark"
                        ] mempty

                    forM_ ports $ \port -> port_ nodeRef port (countPorts port ports)

                    text_
                        [ onDoubleClick $ \e _ -> stopPropagation e : dispatch nodeRef (UI.NodeEvent $ Node.EditExpression nodeId)
                        , "className" $= "name"
                        , "x"         $= "20"
                        , "y"         $= "-16"
                        ] $ elemString $ Text.unpack $ n ^. Node.expression
                    text_
                        [ "className" $= "name"
                        , "x"         $= "20"
                        , "y"         $= "65"
                        ] $ elemString $ strValue n


node_ :: Ref Node -> ReactElementM ViewEventHandler ()
node_ nodeRef = React.view (node nodeRef) () mempty
