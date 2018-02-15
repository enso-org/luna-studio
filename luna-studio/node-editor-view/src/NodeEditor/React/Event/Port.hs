{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}

module NodeEditor.React.Event.Port where

import           Common.Data.Event           (EventName)
import           Common.Prelude
import           LunaStudio.Data.PortDefault (PortDefault)
import           LunaStudio.Data.PortRef     (AnyPortRef (InPortRef'), InPortRef)
import           NodeEditor.Data.Slider      (InitValue)
import           React.Flux                  (KeyboardEvent, MouseEvent)


data Event = Click                   MouseEvent    AnyPortRef
           | MouseDown               MouseEvent    AnyPortRef
           | MouseEnter                            AnyPortRef
           | MouseLeave                            AnyPortRef
           | MouseUp                               AnyPortRef
           | PortApplyString         KeyboardEvent InPortRef  PortDefault
           | EditTextPortControl                   InPortRef  Text
           | EditTextPortControlBlur               InPortRef
           | PortInitSlider          MouseEvent    InPortRef  InitValue
           | PortSetPortDefault                    InPortRef  PortDefault
           deriving (Show, Generic, NFData, Typeable)

instance EventName Event

portRef :: Getter Event AnyPortRef
portRef = to portRef' where
    portRef' (Click                   _ pr)   = pr
    portRef' (MouseDown               _ pr)   = pr
    portRef' (MouseEnter                pr)   = pr
    portRef' (MouseLeave                pr)   = pr
    portRef' (MouseUp                   pr)   = pr
    portRef' (PortApplyString         _ pr _) = InPortRef' pr
    portRef' (EditTextPortControl       pr _) = InPortRef' pr
    portRef' (EditTextPortControlBlur   pr)   = InPortRef' pr
    portRef' (PortInitSlider          _ pr _) = InPortRef' pr
    portRef' (PortSetPortDefault        pr _) = InPortRef' pr

