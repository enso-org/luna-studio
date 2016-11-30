{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module UI.Handlers where

import           Utils.PreludePlus

import           Data.Typeable                    (typeOf)
import           Utils.CtxDynamic

import           Object.Widget                    (DisplayObject, UIHandlers)
import           Object.Widget.Button             (Button)
import           Object.Widget.Choice.RadioButton (RadioButton)
import           Object.Widget.CodeEditor         (CodeEditor)
import           Object.Widget.Connection         (Connection, CurrentConnection)
import           Object.Widget.DataFrame          (DataFrame)
import           Object.Widget.Graphics           (Graphics)
import           Object.Widget.Group              (Group)
import           Object.Widget.Icon               (Icon)
import           Object.Widget.Label              (Label)
import           Object.Widget.LabeledTextBox     (LabeledTextBox)
import           Object.Widget.List               (List)
import           Object.Widget.LongText           (LongText)
import           Object.Widget.Node               (Node, PendingNode)
import           Object.Widget.Number.Continuous  (ContinuousNumber)
import           Object.Widget.Number.Discrete    (DiscreteNumber)
import           Object.Widget.Plots.Image        (Image)
import           Object.Widget.Port               (Port)
import           Object.Widget.Scene              (Scene)
import           Object.Widget.Slider.Continuous  (ContinuousSlider)
import           Object.Widget.Slider.Discrete    (DiscreteSlider)
import           Object.Widget.TextBox            (TextBox)
import           Object.Widget.Toggle             (Toggle)
import           Reactive.State.Global            (State)

import           UI.Widget                        ()
import           UI.Widget.Button                 ()
import           UI.Widget.Choice                 ()
import           UI.Widget.Choice.RadioButton     ()
import           UI.Widget.CodeEditor             ()
import           UI.Widget.Connection             ()
import           UI.Widget.DataFrame              ()
import           UI.Widget.Graphics               ()
import           UI.Widget.Group                  ()
import           UI.Widget.LabeledTextBox         ()
import           UI.Widget.LongText               ()
import           UI.Widget.Node                   ()
import           UI.Widget.Plots.Image            ()
import qualified UI.Widget.Port                   as Port
import           UI.Widget.Slider                 ()
import           UI.Widget.TextBox                ()
import           UI.Widget.Toggle                 ()

import qualified UI.Handlers.Button               as Button
import           UI.Handlers.Choice               ()
import qualified UI.Handlers.Choice.RadioButton   as RadioButton
import qualified UI.Handlers.CodeEditor           as CodeEditor
import qualified UI.Handlers.Connection           as Connection
import qualified UI.Handlers.Label                as Label
import qualified UI.Handlers.LabeledTextBox       as LabeledTextBox
import qualified UI.Handlers.Node                 as Node
import qualified UI.Handlers.Number.Continuous    as ContinuousNumber
import qualified UI.Handlers.Number.Discrete      as DiscreteNumber
import qualified UI.Handlers.Slider.Continuous    as ContinuousSlider
import qualified UI.Handlers.Slider.Discrete      as DiscreteSlider
import qualified UI.Handlers.TextBox              as TextBox
import qualified UI.Handlers.Toggle               as Toggle

class HasHandlers a where
    widgetHandlers :: DisplayObject -> UIHandlers a

nodeType              = typeOf (undefined :: Node)
connectionType        = typeOf (undefined :: Connection)
currentConnectionType = typeOf (undefined :: CurrentConnection)
pendingNodeType       = typeOf (undefined :: PendingNode)
portType              = typeOf (undefined :: Port)

discreteNumberType    = typeOf (undefined :: DiscreteNumber)
continuousNumberType  = typeOf (undefined :: ContinuousNumber)
discreteSliderType    = typeOf (undefined :: DiscreteSlider)
continuousSliderType  = typeOf (undefined :: ContinuousSlider)
toggleType            = typeOf (undefined :: Toggle)
textBoxType           = typeOf (undefined :: TextBox)
labeledTextBoxType    = typeOf (undefined :: LabeledTextBox)
radioButtonType       = typeOf (undefined :: RadioButton)
groupType             = typeOf (undefined :: Group)
buttonType            = typeOf (undefined :: Button)
listType              = typeOf (undefined :: List)
sceneType             = typeOf (undefined :: Scene)
labelType             = typeOf (undefined :: Label)
imageType             = typeOf (undefined :: Image)
longTextType          = typeOf (undefined :: LongText)
dataFrameType         = typeOf (undefined :: DataFrame)
graphicsType          = typeOf (undefined :: Graphics)
iconType              = typeOf (undefined :: Icon)
codeEditorType        = typeOf (undefined :: CodeEditor)

instance HasHandlers State where
    widgetHandlers (CtxDynamic tpe _)
        | tpe == nodeType              =             Node.widgetHandlers
        | tpe == connectionType        =       Connection.widgetHandlers
        | tpe == currentConnectionType = def
        | tpe == pendingNodeType       = def
        | tpe == portType              =             Port.widgetHandlers

        | tpe ==   discreteNumberType  =   DiscreteNumber.widgetHandlers
        | tpe == continuousNumberType  = ContinuousNumber.widgetHandlers
        | tpe ==   discreteSliderType  =   DiscreteSlider.widgetHandlers
        | tpe == continuousSliderType  = ContinuousSlider.widgetHandlers
        | tpe ==           toggleType  =           Toggle.widgetHandlers
        | tpe ==          textBoxType  =          TextBox.widgetHandlers
        | tpe ==   labeledTextBoxType  =   LabeledTextBox.widgetHandlers
        | tpe ==      radioButtonType  =      RadioButton.widgetHandlers
        | tpe ==            groupType  = def
        | tpe ==           buttonType  =           Button.widgetHandlers
        | tpe ==             listType  = def
        | tpe ==            sceneType  = def
        | tpe ==            labelType  =            Label.widgetHandlers
        | tpe ==            imageType  = def
        | tpe ==         longTextType  = def
        | tpe ==        dataFrameType  = def
        | tpe ==         graphicsType  = def
        | tpe ==             iconType  = def
        | tpe ==       codeEditorType  =       CodeEditor.widgetHandlers

        | otherwise                    = error $ "Unknown widget type " <> (show tpe)
