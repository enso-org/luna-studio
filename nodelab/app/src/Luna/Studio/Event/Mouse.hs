module Luna.Studio.Event.Mouse where


import           Data.Position              (Position (Position), Vector2 (Vector2))
import           Luna.Studio.Action.Camera  (getWorkspacePos, translateToWorkspace)
import           Luna.Studio.Action.Command (Command)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global   (State)
import           React.Flux                 (MouseEvent (MouseEvent), mousePageX, mousePageY)
-- import           Data.Bits (setBit, testBit)
-- import           Type.List (Index)
-- import           Data.Typeable


workspacePosition :: MouseEvent -> Command State Position
workspacePosition = translateToWorkspace <=< mousePosition

mousePosition :: MonadIO m => MouseEvent -> m Position
mousePosition e = do
    workspacePos <- getWorkspacePos
    let pagePos = Position (Vector2 (fromIntegral $ mousePageX e) (fromIntegral $ mousePageY e))
    return $ pagePos - workspacePos

leftButton :: Int
leftButton = 0

middleButton :: Int
middleButton = 1

rightButton :: Int
rightButton = 2

withoutMods :: MouseEvent -> Int -> Bool
withoutMods (MouseEvent False b _ _ _ False _ False _ _ _ _ _ False) button = button == b
withoutMods _ _                                                             = False

withCtrl :: MouseEvent -> Int -> Bool
withCtrl (MouseEvent False b _ _ _ True _ _    _ _ _ _ _ False) button = button == b
withCtrl (MouseEvent False b _ _ _ _    _ True _ _ _ _ _ False) button = button == b
withCtrl _ _                                                           = False

withAlt :: MouseEvent -> Int -> Bool
withAlt (MouseEvent True b _ _ _ False _ False _ _ _ _ _ False) button = button == b
withAlt _ _                                                            = False

withShift :: MouseEvent -> Int -> Bool
withShift (MouseEvent False b _ _ _ False _ False _ _ _ _ _ True) button = button == b
withShift _ _                                                            = False

withCtrlAlt :: MouseEvent -> Int -> Bool
withCtrlAlt (MouseEvent True b _ _ _ True _ _    _ _ _ _ _ False) button = button == b
withCtrlAlt (MouseEvent True b _ _ _ _    _ True _ _ _ _ _ False) button = button == b
withCtrlAlt _ _                                                          = False

withCtrlShift :: MouseEvent -> Int -> Bool
withCtrlShift (MouseEvent False b _ _ _ True _ _    _ _ _ _ _ True) button = button == b
withCtrlShift (MouseEvent False b _ _ _ _    _ True _ _ _ _ _ True) button = button == b
withCtrlShift _ _                                                          = False

withAltShift :: MouseEvent -> Int -> Bool
withAltShift (MouseEvent True b _ _ _ False _ False _ _ _ _ _ True) button = button == b
withAltShift _ _                                                           = False

withCtrlAltShift :: MouseEvent -> Int -> Bool
withCtrlAltShift (MouseEvent True b _ _ _ True _ _    _ _ _ _ _ True) button = button == b
withCtrlAltShift (MouseEvent True b _ _ _ _    _ True _ _ _ _ _ True) button = button == b
withCtrlAltShift _ _                                                         = False


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
