{-# LANGUAGE DeriveAnyClass #-}
module NodeEditor.React.Event.Node where

import           Common.Prelude
import           LunaStudio.Data.NodeLoc     (NodeLoc)
import           LunaStudio.Data.PortDefault (PortDefault)
import           LunaStudio.Data.PortRef     (InPortRef)
import           NodeEditor.State.Action     (InitValue)
import           React.Flux                  (KeyboardEvent, MouseEvent)


data Event = DisplayResultChanged Bool          NodeLoc
           | EditExpression                     NodeLoc
           | EditName                           NodeLoc
           | Enter                              NodeLoc
           | MouseDown            MouseEvent    NodeLoc
           | PortApplyString      KeyboardEvent InPortRef PortDefault
           | PortEditString                     InPortRef PortDefault
           | PortInitSlider          MouseEvent InPortRef InitValue
           | PortSetPortDefault                 InPortRef PortDefault
           | Select               MouseEvent    NodeLoc
           | SetExpression                      NodeLoc Text
            deriving (Show, Generic, NFData, Typeable)
