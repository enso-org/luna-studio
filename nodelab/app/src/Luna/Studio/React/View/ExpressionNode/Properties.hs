{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.View.ExpressionNode.Properties where

import qualified JS.Config                                             as Config
import qualified JS.UI                                                 as UI
import qualified Luna.Studio.Event.UI                                  as UI
import           Luna.Studio.Prelude
import qualified Luna.Studio.React.Event.Node                          as Node
import           Luna.Studio.React.Model.App                           (App)
import qualified Luna.Studio.React.Model.Field                         as Field
import           Luna.Studio.React.Model.Node.ExpressionNodeProperties (NodeProperties)
import qualified Luna.Studio.React.Model.Node.ExpressionNodeProperties as Prop
import           Luna.Studio.React.Store                               (Ref, dispatch)
import           Luna.Studio.React.View.Field                          (singleField_)
import           Luna.Studio.React.View.PortControl                    (portControl_)
import qualified Luna.Studio.React.View.Style                          as Style
import           React.Flux
import qualified React.Flux                                            as React


objName :: JSString
objName = "node-properties"

nodeProperties :: ReactView (Ref App, NodeProperties)
nodeProperties = React.defineView objName $ \(ref, p) -> do
    let nodeId = p ^. Prop.nodeId
    div_
        [ "key"       $= "properties"
        , "className" $= Style.prefixFromList [ "node__properties", "noselect" ]
        ] $ do
        div_
            [ "key"       $= "value"
            , "className" $= Style.prefixFromList [ "row", "row--output-name" ]
            , onDoubleClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.NameEditStart nodeId
            ] $
            case p ^. Prop.nameEdit of
                Just name ->
                    singleField_ ["id"  $= nameLabelId] "name-label"
                        $ Field.mk ref name
                        & Field.onCancel .~ Just (const $ UI.NodeEvent $ Node.NameDiscard nodeId)
                        & Field.onAccept .~ Just (const $ UI.NodeEvent $ Node.NameApply nodeId)
                        & Field.onEdit   .~ Just (UI.NodeEvent . flip Node.NameChange nodeId)
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
                div_
                    [ "key" $= "ctrl-switch"
                    , "className" $= Style.prefixFromList (["ctrl-switch"] ++ if val then ["ctrl-switch--on"] else ["ctrl-switch--off"])
                    , onClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.DisplayResultChanged (not val) nodeId
                    ] mempty
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
