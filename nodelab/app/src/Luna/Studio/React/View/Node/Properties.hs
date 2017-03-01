{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.Node.Properties where

import qualified JS.Config                              as Config
import qualified JS.UI                                  as UI
import           React.Flux
import qualified React.Flux                             as React
import qualified Luna.Studio.Event.UI                   as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Node           as Node
import           Luna.Studio.React.Model.App            (App)
import           Luna.Studio.React.Model.NodeProperties (NodeProperties)
import qualified Luna.Studio.React.Model.NodeProperties as Prop
import qualified Luna.Studio.React.View.Style           as Style
import           Luna.Studio.React.Store                (Ref, dispatch)
import           Luna.Studio.React.View.PortControl     (portControl_)

objName :: JSString
objName = "node-properties"

nodeProperties :: ReactView (Ref App, NodeProperties)
nodeProperties = React.defineView objName $ \(ref, p) -> do
    let nodeId = p ^. Prop.nodeId
    div_
        [ "key"       $= "properties"
        , "className" $= Style.prefixFromList ["node__properties", "noselect"]
        ] $ do
        div_
            [ "key"       $= "value"
            , "className" $= Style.prefixFromList [ "row", "row--output-name" ]
            , onDoubleClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.NameEditStart nodeId
            ] $
            case (p ^. Prop.nameEdit) of
                Just name ->
                    input_
                        [ "key" $= "name-label"
                        , "id"  $= "focus-nameLabel"
                        , Style.prefixFromList [ "value", "value--name" ] $= convert name
                        , onMouseDown $ \e _ -> [stopPropagation e]
                        , onKeyDown   $ \e k ->  stopPropagation e : dispatch ref (UI.NodeEvent $ Node.NameKeyDown k nodeId)
                        , onChange    $ \e -> let val = target e "value" in dispatch ref $ UI.NodeEvent $ Node.NameChange (fromString val) nodeId
                        ]
                Nothing ->
                    elemString $ convert $ p ^. Prop.name
        forM_ (p ^. Prop.ports) $ portControl_ ref nodeId (p ^. Prop.isLiteral)
        div_
            [ "key"       $= "display-results"
            , "className" $= Style.prefix "row"
            ] $ do
            div_
                [ "key"       $= "label"
                , "className" $= Style.prefix "label"
                ] $ elemString "Display results"
            div_
                [ "key"       $= "value"
                , "className" $= Style.prefix "value"
                ] $ do
                let val = p ^. Prop.visualizationsEnabled
                button_
                    [ "key" $= "button"
                    , onClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.DisplayResultChanged (not val) nodeId
                    ] $ elemString $ if val then "yes" else "no"
        div_
            [ "key" $= "execution-time"
            , "className" $= Style.prefix "row"
            ] $
            withJust (p ^. Prop.execTime) $ \execTime -> do
                div_
                    ["key"       $= "label"
                    , "className" $= Style.prefix "label"
                    ] $ elemString "Execution time"
                div_
                    ["key"       $= "value"
                    , "className" $= Style.prefix "value"
                    ] $ elemString $ show execTime <> " ms"



nodeProperties_ :: Ref App -> NodeProperties -> ReactElementM ViewEventHandler ()
nodeProperties_ ref prop = React.viewWithSKey nodeProperties objName (ref, prop) mempty

nameLabelId :: JSString
nameLabelId = Config.prefix "focus-nameLabel"

focusNameLabel :: IO ()
focusNameLabel = UI.focus nameLabelId
