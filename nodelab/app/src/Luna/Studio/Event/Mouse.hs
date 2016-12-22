module Luna.Studio.Event.Mouse where


import           Luna.Studio.Data.Vector (Position, Vector2 (Vector2))
import           Luna.Studio.Prelude
import           React.Flux              (MouseEvent (MouseEvent), mousePageX, mousePageY)

--TODO[react]: Apply zoom and pan
getMousePosition :: MouseEvent -> Position
getMousePosition e = Vector2 (fromIntegral $ mousePageX e) (fromIntegral $ mousePageY e)

withoutMods :: MouseEvent -> Bool
withoutMods (MouseEvent False _ _ _ _ False _ False _ _ _ _ _ False) = True
withoutMods _                                                        = False

withCtrl :: MouseEvent -> Bool
withCtrl (MouseEvent False _ _ _ _ True _ _    _ _ _ _ _ False) = True
withCtrl (MouseEvent False _ _ _ _ _    _ True _ _ _ _ _ False) = True
withCtrl _                                                      = False

withAlt :: MouseEvent -> Bool
withAlt (MouseEvent True _ _ _ _ False _ False _ _ _ _ _ False) = True
withAlt _                                                       = False

withShift :: MouseEvent -> Bool
withShift (MouseEvent False _ _ _ _ False _ False _ _ _ _ _ True) = True
withShift _                                                       = False

withCtrlAlt :: MouseEvent -> Bool
withCtrlAlt (MouseEvent True _ _ _ _ True _ _    _ _ _ _ _ False) = True
withCtrlAlt (MouseEvent True _ _ _ _ _    _ True _ _ _ _ _ False) = True
withCtrlAlt _                                                     = False

withCtrlShift :: MouseEvent -> Bool
withCtrlShift (MouseEvent False _ _ _ _ True _ _    _ _ _ _ _ True) = True
withCtrlShift (MouseEvent False _ _ _ _ _    _ True _ _ _ _ _ True) = True
withCtrlShift _                                                     = False

withAltShift :: MouseEvent -> Bool
withAltShift (MouseEvent True _ _ _ _ False _ False _ _ _ _ _ True) = True
withAltShift _                                                      = False

withCtrlAltShift :: MouseEvent -> Bool
withCtrlAltShift (MouseEvent True _ _ _ _ True _ _    _ _ _ _ _ True) = True
withCtrlAltShift (MouseEvent True _ _ _ _ _    _ True _ _ _ _ _ True) = True
withCtrlAltShift _                                                    = False
