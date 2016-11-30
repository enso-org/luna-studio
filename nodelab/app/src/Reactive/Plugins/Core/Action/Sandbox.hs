{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Sandbox where

import           Utils.PreludePlus
import           Utils.Vector

import           Event.Event                     (Event (..))
import qualified Event.Keyboard                  as Keyboard
import           Reactive.Commands.Command       (Command, performIO)
import qualified Reactive.State.Global           as Global

import qualified Reactive.Commands.UIRegistry    as UICmd
import           Reactive.State.UIRegistry       (addHandler, sceneGraphId)

import qualified Object.Widget.CodeEditor        as CodeEditor
import qualified Object.Widget.Graphics          as G
import qualified Object.Widget.Group             as Group
import qualified Object.Widget.LabeledTextBox    as LabeledTextBox
import qualified Object.Widget.List              as List
import qualified Object.Widget.Number.Continuous as ContinuousNumber
import qualified Object.Widget.Number.Discrete   as DiscreteNumber
import qualified Object.Widget.Slider.Continuous as ContinuousSlider
import qualified Object.Widget.Slider.Discrete   as DiscreteSlider
import qualified Object.Widget.Toggle            as Toggle

import qualified Reactive.Commands.Batch         as BatchCmd

import           Object.LunaValue
import           Object.LunaValue.Instances      ()
import           Reactive.State.Global           (inRegistry)
import           UI.Handlers.Generic             (ValueChangedHandler (..))
import           UI.Layout                       as Layout


f1, f2 :: Char
f1 = '\112'
f2 = '\113'

toAction :: Event -> Maybe (Command Global.State ())
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\112' _)) = Just $ Global.inRegistry $ do
    performIO $ putStrLn "show sandbox"
    let groupWidget = Group.create
    parent <- UICmd.register sceneGraphId groupWidget (Layout.verticalLayoutHandler 5.0)
    let register w = UICmd.register_ parent w def

    register $ DiscreteNumber.create (Vector2 200 20) "Discrete" 42
    register $ ContinuousNumber.create (Vector2 200 20) "Continuous" 42.42
    register $ ContinuousSlider.create (Vector2 200 20) "ContinuousSlider" (-2.0) 5.0 3.0
    register $ DiscreteSlider.create (Vector2 200 20) "DiscreteSlider" (-15) 80 30
    register $ DiscreteSlider.create (Vector2 200 20) "DiscreteSlider" 3 8 4
    register $ CodeEditor.create (Vector2 300 150) "class Point:\n    x y z :: Int\n  origin      = Point 0 0 0\n  Point x y _ = origin\n  print 'Origin XY coords are ($x,$y)'"
    register $ Toggle.create (Vector2 200 20) "Toggle" True
    register $ LabeledTextBox.create (Vector2 200 20) "Textbox" "foo"

    let values = (AnyLunaValue <$> ([1, 2, 3, 4, 5, 6, 7] :: [Int]))
              <> (AnyLunaValue <$> ([0.1, 0.2, 0.3] :: [Double]))
              <> (AnyLunaValue <$> [True, False, True])
    register $ List.createList 200 "List" values $ AnyLunaValue (-1 :: Int)
    return ()

toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\116' _)) = Just $ Global.inRegistry $ do
    performIO $ putStrLn "show sandbox"
    let groupWidget = Group.create
    parent'       <- UICmd.register sceneGraphId groupWidget (Layout.horizontalLayoutHandler 5.0)
    parent        <- UICmd.register parent'      groupWidget (Layout.verticalLayoutHandler 5.0)
    resizedParent <- UICmd.register parent'      groupWidget (Layout.verticalLayoutHandler 5.0)
    flexParent    <- UICmd.register parent'      groupWidget (Layout.flexVerticalLayoutHandler 5.0)

    let widget1 = ContinuousSlider.create (Vector2 200 20) "ContinuousSlider" (-2.0) 5.0 3.0
    resizableWidget  <- UICmd.register resizedParent widget1 def
    _resizableWidget2 <- UICmd.register resizedParent widget1 def
    resizableWidget3 <- UICmd.register resizedParent widget1 def
    UICmd.register_ flexParent widget1 def
    UICmd.register_ flexParent widget1 def
    UICmd.register_ flexParent widget1 def
    UICmd.register_ flexParent widget1 def
    UICmd.register_ flexParent widget1 def
    UICmd.register_ flexParent widget1 def
    -- UICmd.moveX resizedParent 200

    let widget2 = ContinuousNumber.create (Vector2 200 20) "Width" 200
    UICmd.register_ parent widget2 $ addHandler (ValueChangedHandler $ \val _ -> inRegistry $ do
        UICmd.resize' resizableWidget  (x .~ val)
        UICmd.resize' resizableWidget3 (x .~ val)
        ) mempty

    let widget3 = ContinuousNumber.create (Vector2 200 20) "Height" 20
    UICmd.register_ parent widget3 $ addHandler (ValueChangedHandler $ \val _ -> inRegistry $ do
        UICmd.resize' resizableWidget  (y .~ val)
        UICmd.resize' resizableWidget3 (y .~ val)
        ) mempty

    let widget4 = ContinuousNumber.create (Vector2 200 20) "FlexHeight" 100
    UICmd.register_ parent widget4 $ addHandler (ValueChangedHandler $ \val _ -> inRegistry $ do
        UICmd.resize' flexParent (y .~ val)
        ) mempty

    return ()
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\115' _)) = Just $ Global.inRegistry $ do
    performIO $ putStrLn "show sandbox"
    performIO $ putStrLn "show sandbox"
    let groupWidget = Group.create
    parent <- UICmd.register sceneGraphId groupWidget (Layout.verticalLayoutHandler 5.0)

    let shader  = "void main() { gl_FragColor = vec4(0.1, 0.0, 0.0, 1.0); } "
        shader2 = "void main() { gl_FragColor = vec4(1.0, 1.0, 0.0, 1.0); } "
        n       = 4.0
        n'      = 4
        items   = [ G.Item shader  [ G.Box (Vector2 0.0 0.0) ] (Vector2 1.0 1.0) (Vector2 0.0 0.0)
                  , G.Item shader2 [ G.Box (Vector2 (i / n) 0.0) | i <- [1..n']] (Vector2 1.0 1.0) (Vector2 0.0 0.0)
                  ]
        labels  = []
        widget = G.create (Vector2 400 400) items labels
    UICmd.register_ parent widget def

    return ()

toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\113' _)) = Just BatchCmd.dumpGraphViz

toAction _  = Nothing
