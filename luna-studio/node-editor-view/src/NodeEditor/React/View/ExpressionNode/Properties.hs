{-# LANGUAGE OverloadedStrings #-}
module NodeEditor.React.View.ExpressionNode.Properties where

import           Common.Prelude
import qualified Data.Aeson                                           as Aeson
import           LunaStudio.Data.Constants                            (gridSize)
import           LunaStudio.Data.PortRef                              (AnyPortRef (InPortRef'), InPortRef, toAnyPortRef)
import           NodeEditor.React.IsRef                               (IsRef)
import           NodeEditor.React.Model.Node.ExpressionNodeProperties (NodeProperties)
import qualified NodeEditor.React.Model.Node.ExpressionNodeProperties as Prop
import qualified NodeEditor.React.Model.Port                          as Port
import           NodeEditor.React.View.PortControl                    (portControl_)
import qualified NodeEditor.React.View.Style                          as Style
import           React.Flux
import qualified React.Flux                                           as React

objName :: JSString
objName = "node-properties"

nodeProperties :: IsRef ref => ReactView (ref, NodeProperties, Maybe InPortRef, Int)
nodeProperties = React.defineView objName $ \(ref, prop, mayEditedTextPortControlPortRef, numOfOutputs) -> do
    let nodeLoc    = prop ^. Prop.nodeLoc
        ports      = if prop ^. Prop.isExpanded && null (Prop.inPortsList prop) then maybeToList $ prop ^? Prop.inPortAt [] else Prop.inPortsList prop
        controls p = portControl_ ref nodeLoc p ((InPortRef' <$> mayEditedTextPortControlPortRef) == Just (toAnyPortRef nodeLoc $ p ^. Port.portId))
    div_
        [ "key"       $= "controls"
        , "className" $= Style.prefixFromList [ "node__controls", "noselect" ]
        , "style"     @= Aeson.object [ "minHeight" Aeson..= ((show $ (toEnum numOfOutputs) * gridSize) <> "rem") ]
        ] $ forM_ ports $ controls

nodeProperties_ :: IsRef ref => ref -> NodeProperties -> Maybe InPortRef -> Int -> ReactElementM ViewEventHandler ()
nodeProperties_ ref prop mayEditedTextPortControlPortRef numOfOutputs = React.viewWithSKey nodeProperties objName (ref, prop, mayEditedTextPortControlPortRef, numOfOutputs) mempty
