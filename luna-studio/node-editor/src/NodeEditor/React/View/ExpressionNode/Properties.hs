{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.ExpressionNode.Properties where

import           Common.Prelude
import           NodeEditor.React.Model.App                           (App)
import           NodeEditor.React.Model.Node.ExpressionNodeProperties (NodeProperties)
import qualified NodeEditor.React.Model.Node.ExpressionNodeProperties as Prop
import           NodeEditor.React.Store                               (Ref)
import           NodeEditor.React.View.PortControl                    (portLabel_, portControl_)
import qualified NodeEditor.React.View.Style                          as Style
import           React.Flux
import qualified React.Flux                                            as React

objName :: JSString
objName = "node-properties"

nodeProperties :: ReactView (Ref App, NodeProperties)
nodeProperties = React.defineView objName $ \(ref, p) -> do
    let nodeLoc    = p ^. Prop.nodeLoc
        controls p = div_
                        [ "className" $= Style.prefix "node__control"
                        ] $ do
                        portLabel_ p
                        portControl_ ref nodeLoc p
    div_
        [ "key"       $= "controls"
        , "className" $= Style.prefixFromList [ "node__controls", "noselect" ]
        ] $ do
        div_
           [ "key"       $= "label-self"
           , "className" $= Style.prefixFromList [ "node__label", "node__label--self" ]
           ] $ elemString "self"
        forM_ (Prop.inPortsList p) $ controls
        -- div_
        --     [ "key"       $= "showResults"
        --     , "className" $= Style.prefix "row"
        --     ] $ do
        --     div_
        --         [ "key"       $= "label"
        --         , "className" $= Style.prefix "label"
        --         ] $ elemString "Show results"
        --     div_
        --         [ "key"       $= "value"
        --         , "className" $= Style.prefix "value"
        --         ] $ do
        --         let val = p ^. Prop.visualizationsEnabled
        --         div_
        --             [ "key" $= "ctrlSwitch"
        --             , "className" $= Style.prefixFromList (["ctrl-switch"] ++ if val then ["ctrl-switch--on"] else ["ctrl-switch--off"])
        --             , onDoubleClick $ \e _ -> [stopPropagation e]
        --             , onClick       $ \_ _ -> dispatch ref $ UI.NodeEvent $ Node.DisplayResultChanged (not val) nodeLoc
        --             ] mempty
        -- div_
        --     [ "key" $= "executionTime"
        --     , "className" $= Style.prefix "row"
        --     ] $
        --     withJust (p ^. Prop.execTime) $ \execTime -> do
        --         div_
        --             ["key"       $= "label"
        --             , "className" $= Style.prefix "label"
        --             ] $ elemString "Execution time"
        --         div_
        --             ["key"       $= "value"
        --             , "className" $= Style.prefix "value"
        --             ] $ elemString $ show execTime <> " ms"

nodeProperties_ :: Ref App -> NodeProperties -> ReactElementM ViewEventHandler ()
nodeProperties_ ref prop = React.viewWithSKey nodeProperties objName (ref, prop) mempty
