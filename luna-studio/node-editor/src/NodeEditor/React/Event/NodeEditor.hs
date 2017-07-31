{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Event.NodeEditor where

import           React.Flux     (MouseEvent, WheelEvent)

import           Common.Prelude



data Event = ContextMenu
           | MouseDown  MouseEvent
           | Wheel      MouseEvent WheelEvent
            deriving (Show, Generic, NFData, Typeable)
