{-# LANGUAGE OverloadedStrings #-}
module Node.Editor.React.View.ExpressionNode.Properties where

import qualified Node.Editor.Event.UI                                  as UI
import           Luna.Prelude
import qualified Node.Editor.React.Event.Node                          as Node
import           Node.Editor.React.Model.App                           (App)
import           Node.Editor.React.Model.Node.ExpressionNodeProperties (NodeProperties)
import qualified Node.Editor.React.Model.Node.ExpressionNodeProperties as Prop
import           Node.Editor.React.Store                               (Ref, dispatch)
import           Node.Editor.React.View.PortControl                    (portControl_)
import qualified Node.Editor.React.View.Style                          as Style
import           React.Flux
import qualified React.Flux                                            as React

objName :: JSString
objName = "node-properties"

nodeProperties :: ReactView (Ref App, NodeProperties)
nodeProperties = React.defineView objName $ \(ref, p) -> do
    let nodeLoc = p ^. Prop.nodeLoc
    div_
        [ "key"       $= "properties"
        , "className" $= Style.prefixFromList [ "node__properties", "noselect" ]
        ] $ do
        div_
            [ "key"       $= "value"
            , "className" $= Style.prefixFromList [ "row", "row--first" ]
            --, onDoubleClick $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.NameEditStart nodeLoc
            ] $ mempty
        forM_ (Prop.inPortsList p) $ portControl_ ref nodeLoc
        div_
            [ "key"       $= "showResults"
            , "className" $= Style.prefix "row"
            ] $ do
            div_
                [ "key"       $= "label"
                , "className" $= Style.prefix "label"
                ] $ elemString "Show results"
            div_
                [ "key"       $= "value"
                , "className" $= Style.prefix "value"
                ] $ do
                let val = p ^. Prop.visualizationsEnabled
                div_
                    [ "key" $= "ctrlSwitch"
                    , "className" $= Style.prefixFromList (["ctrl-switch"] ++ if val then ["ctrl-switch--on"] else ["ctrl-switch--off"])
                    , onDoubleClick $ \e _ -> [stopPropagation e]
                    , onClick       $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.DisplayResultChanged (not val) nodeLoc
                    ] mempty
        div_
            [ "key" $= "executionTime"
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
