{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.React.Event.Node where

import           Common.Prelude
import           Common.Data.Event           (EventName)
import           LunaStudio.Data.NodeLoc     (NodeLoc)
import           LunaStudio.Data.PortDefault (PortDefault)
import           LunaStudio.Data.PortRef     (InPortRef)
import           NodeEditor.Data.Slider      (InitValue)
import           React.Flux                  (KeyboardEvent, MouseEvent)


data Event = EditExpression                     NodeLoc
           | EditName                           NodeLoc
           | Enter                              NodeLoc
           | MouseDown            MouseEvent    NodeLoc
           | PortApplyString      KeyboardEvent InPortRef PortDefault
           | EditTextPortControl                InPortRef Text
           | EditTextPortControlBlur
           | PortInitSlider       MouseEvent    InPortRef InitValue
           | PortSetPortDefault                 InPortRef PortDefault
           | Select               MouseEvent    NodeLoc
           | SetExpression                      NodeLoc Text
           | MouseEnter                         NodeLoc
           | MouseLeave                         NodeLoc
           | ShowFullError                      NodeLoc
            deriving (Show, Generic, NFData, Typeable)

instance EventName Event
