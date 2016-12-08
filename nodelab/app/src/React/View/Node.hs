{-# LANGUAGE OverloadedStrings #-}
module React.View.Node where


import qualified Data.Text.Lazy    as Text

import           React.Flux
import qualified React.Flux        as React

import qualified Event.UI          as UI
import           React.Store       (Ref, dispatch, dt)
import           React.Store.Node  (Node)
import qualified React.Store.Node  as Node
import           React.View.Port   (port_)
import           Utils.PreludePlus
import           Utils.Vector      (x, y)

name :: JSString
name = "node-editor"

--FIXME: move all styles to CSS
node :: Ref Node -> ReactView ()
node nodeRef = React.defineControllerView
    name nodeRef $ \nodeStore () -> do
        let n = nodeStore ^. dt
            nodeId = n ^. Node.nodeId
            pos = n ^. Node.position
            translate = fromString $ "translate(" <> show (pos ^. x) <> "," <> show (pos ^. y) <> ")" -- TODO: Consider implementing matrices
        g_
            [ onClick       $ \_ m -> dispatch nodeRef $ UI.NodeEvent $ Node.Select m nodeId
            , onDoubleClick $ \_ _ -> dispatch nodeRef $ UI.NodeEvent $ Node.Enter nodeId
            , onMouseDown   $ \e m -> stopPropagation e : dispatch nodeRef (UI.NodeEvent $ Node.MouseDown m nodeId)
            , "className" $= (fromString $ "node node--collapsed" <> (if n ^. Node.isSelected then " node--selected" else []))
            , "transform" $= translate
            , "key"       $= fromString (show nodeId)
            ] $ do
                text_
                    [ "className" $= "name"
                    , "x"         $= "22" -- FIXME: half of the node width
                    , "y"         $= "-16"
                    ] $ elemString $ Text.unpack $ n ^. Node.name

                forM_ (n ^. Node.ports) $ port_ nodeRef


node_ :: Ref Node -> ReactElementM ViewEventHandler ()
node_ nodeRef = React.view (node nodeRef) () mempty
