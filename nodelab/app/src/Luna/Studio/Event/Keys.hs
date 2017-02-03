module Luna.Studio.Event.Keys where

import           Luna.Studio.Prelude

import           React.Flux          (KeyboardEvent (KeyboardEvent))


a :: Int
a = 65

h :: Int
h = 72

y :: Int
y = 89

z :: Int
z = 90

plus :: Int
plus = 187

minus :: Int
minus = 189

zero :: Int
zero = 48

del :: Int
del = 46

tab :: Int
tab = 9

enter :: Int
enter = 13

esc :: Int
esc = 27

leftArrow :: Int
leftArrow = 37

upArrow :: Int
upArrow = 38

rightArrow :: Int
rightArrow = 39

downArrow :: Int
downArrow = 40

withoutMods :: KeyboardEvent -> Int -> Bool
withoutMods (KeyboardEvent False _ False _ _ keyCode _ _ False _ False _) key = keyCode == key
withoutMods _ _                                                               = False

withCtrl :: KeyboardEvent -> Int -> Bool
withCtrl (KeyboardEvent False _ True  _ _ keyCode _ _ _    _ False _) key = keyCode == key
withCtrl (KeyboardEvent False _ False _ _ keyCode _ _ True _ False _) key = keyCode == key
withCtrl _ _                                                              = False

withAlt :: KeyboardEvent -> Int -> Bool
withAlt (KeyboardEvent True _ False _ _ keyCode _ _ False _ False _) key = keyCode == key
withAlt _ _                                                              = False

withShift :: KeyboardEvent -> Int -> Bool
withShift (KeyboardEvent False _ False _ _ keyCode _ _ False _ True _) key = keyCode == key
withShift _ _                                                              = False

withCtrlAlt :: KeyboardEvent -> Int -> Bool
withCtrlAlt (KeyboardEvent True _ True  _ _ keyCode _ _ _    _ False _) key = keyCode == key
withCtrlAlt (KeyboardEvent True _ False _ _ keyCode _ _ True _ False _) key = keyCode == key
withCtrlAlt _ _                                                             = False

withCtrlShift :: KeyboardEvent -> Int -> Bool
withCtrlShift (KeyboardEvent False _ True  _ _ keyCode _ _ _    _ True _) key = keyCode == key
withCtrlShift (KeyboardEvent False _ False _ _ keyCode _ _ True _ True _) key = keyCode == key
withCtrlShift _ _                                                             = False

withAltShift :: KeyboardEvent -> Int -> Bool
withAltShift (KeyboardEvent True _ False _ _ keyCode _ _ False _ True _) key = keyCode == key
withAltShift _ _                                                             = False

withCtrlAltShift :: KeyboardEvent -> Int -> Bool
withCtrlAltShift (KeyboardEvent True _ True  _ _ keyCode _ _ _    _ False _) key = keyCode == key
withCtrlAltShift (KeyboardEvent True _ False _ _ keyCode _ _ True _ False _) key = keyCode == key
withCtrlAltShift _ _                                                             = False
