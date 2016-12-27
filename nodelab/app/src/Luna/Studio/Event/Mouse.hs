module Luna.Studio.Event.Mouse where


import           Luna.Studio.Data.Vector (Position (Position), Vector2 (Vector2))
import           Luna.Studio.Prelude
import           React.Flux              (MouseEvent (MouseEvent), mousePageX, mousePageY)
-- import           Data.Bits (setBit, testBit)
-- import           Type.List (Index)
-- import           Data.Typeable


--TODO[react]: implement, apply zoom and pan
workspacePosition :: MouseEvent -> Position
workspacePosition = mousePosition

mousePosition :: MouseEvent -> Position
mousePosition e = Position (Vector2 (fromIntegral $ mousePageX e) (fromIntegral $ mousePageY e))

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


-- TODO[react]: Consider rewrite with below approach. Also we need to find a way
--              to use regular key as mod to MouseEvent. We should talk about
--              this and keeping full state of Keyboard and Mouse with Wojciech. 
-- data ModType = Alt
--              | Ctrl
--              | Shift
--              deriving (Enum)
--
--
-- newtype Mods = Mods Word64 deriving (Bits, Default)
--
-- setMod :: ModType -> Mods -> Mods
-- setMod = flip setBit . fromEnum
--
-- setMods = foldl setMod
--
-- checkMod :: ModType -> Mods -> Bool
-- checkMod = testBit setBit . fromEnum
--
-- checkMods = foldl (&&) checkMod
--
-- only = setMods def
--
--
--
--
-- m = def :: Mods
-- m2 = setMod Ctrl m
-- m3 = setMod Shift m2
--
--
-- checkMods [Alt, Ctrl] m3
--
--
--
-- x == only [Alt, Ctrl]
--
--
--
-- foo me | me == only [Alt, Ctrl] = ...
--        | me == only [Alt]       = ..
--
