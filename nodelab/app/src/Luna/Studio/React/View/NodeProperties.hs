{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.NodeProperties where

import qualified Data.Text                              as Text
import qualified Luna.Studio.Event.UI                   as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Node           as Node
import           Luna.Studio.React.Model.App            (App)
import           Luna.Studio.React.Model.NodeProperties (NodeProperties)
import qualified Luna.Studio.React.Model.NodeProperties as Prop
import           Luna.Studio.React.Store                (Ref, dispatch)
import           Luna.Studio.React.View.CommonElements  (blurBackground_)
import           Luna.Studio.React.View.PortControl     (portControl_)
import           React.Flux
import qualified React.Flux                             as React


objName :: JSString
objName = "node-properties"

nodeProperties :: ReactView (Ref App, NodeProperties)
nodeProperties = React.defineView objName $ \(ref, p) -> do
    let nodeId = p ^. Prop.nodeId
    div_
        [ "key"       $= "properties"
        , "className" $= "node__properties"
        ] $ do
        blurBackground_
        div_
            [ "key"       $= "value"
            , "className" $= "row row--output-name"
            , onDoubleClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.NameEditStart nodeId
            ] $ do
            case (p ^. Prop.nameEdit) of
                Just name ->
                    input_
                        [ "key" $= "name-label"
                        , "id"  $= "focus-nameLabel"
                        , "value value--name" $= fromString (Text.unpack name)
                        , onMouseDown $ \e _ -> [stopPropagation e]
                        , onKeyDown   $ \e k ->  stopPropagation e : dispatch ref (UI.NodeEvent $ Node.NameKeyDown k nodeId)
                        , onChange    $ \e -> let val = target e "value" in dispatch ref $ UI.NodeEvent $ Node.NameChange (fromString val) nodeId
                        ]
                Nothing ->
                    elemString $ fromString $ Text.unpack $ (p ^. Prop.name)
        forM_ (p ^. Prop.ports) $ portControl_ ref nodeId (p ^. Prop.isLiteral)
        div_
            [ "key"       $= "display-results"
            , "className" $= "row"
            ] $ do
            div_
                [ "key"       $= "label"
                , "className" $= "label"
                ] $ elemString "Display results"
            div_
                [ "key"       $= "value"
                , "className" $= "value"
                ] $ do
                let val = p ^. Prop.visualizationsEnabled
                button_
                    [ "key" $= "button"
                    , onClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.DisplayResultChanged (not val) nodeId
                    ] $ elemString $ fromString $ if val then "yes" else "no"
        div_
            [ "key" $= "execution-time"
            , "className" $= "row"
            ] $ do
            withJust (p ^. Prop.execTime) $ \execTime -> do
                div_
                    ["key"       $= "label"
                    , "className" $= "label"
                    ] $ elemString "Execution time"
                div_
                    ["key"       $= "value"
                    , "className" $= "value"
                    ] $ elemString $ show execTime <> " ms"

nodeProperties_ :: Ref App -> NodeProperties -> ReactElementM ViewEventHandler ()
nodeProperties_ ref prop = React.viewWithSKey nodeProperties objName (ref, prop) mempty

foreign import javascript safe "document.getElementById('focus-nameLabel').focus()" focusNameLabel :: IO ()
