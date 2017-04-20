{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.Plane where

import qualified NodeEditor.React.View.Style as Style
import           React.Flux                   hiding (transform)



svgPlanes_ :: ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
svgPlanes_ =
    svg_
        [ "className" $= Style.prefix "svg-planes"
        , "key"       $= "svgPlanes"
        ]

planeMonads_ :: ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
planeMonads_ =
    g_
        [ "className" $= Style.prefixFromList [ "plane", "plane--monads", "camera-transform" ]
        , "key"       $= "monads"
        ]

planeConnections_ :: ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
planeConnections_ =
    g_
        [ "className" $= Style.prefixFromList [ "plane", "plane--connections", "camera-transform" ]
        , "key"       $= "connections"
        ]

planeNodes_ :: ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
planeNodes_ =
    div_
        [ "className" $= Style.prefixFromList [ "plane", "plane--nodes" ]
        , "key"       $= "nodes"
        ]

planeCanvas_ :: ReactElementM ViewEventHandler () -> ReactElementM ViewEventHandler ()
planeCanvas_ =
    canvas_
        [ "className" $= Style.prefixFromList [ "plane", "plane--canvas", "hide" ]
        , "key"       $= "canvas"
        ]
