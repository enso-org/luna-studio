module Style.Node where

import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector
import           Data.HMap.Lazy               (HTMap)

import           Style.Types

import qualified Object.Widget.Group as Group
import qualified Object.Widget.Label as Label
import qualified UI.Layout           as Layout

nodeRadius :: Double
nodeRadius = 25.0

expressionLabel, valueLabel :: Text -> Label.Label
expressionLabel = Label.Label position size align Label.Monospace where
    position = Vector2 (-150.0) (-50.0)
    size     = Vector2 300.0 20.0
    align    = Label.Center

controlsPosition :: Vector2 Double
controlsPosition = Vector2 (-nodeRadius) 35.0

controlsLayout, inLabelsLayout, expandedGroupLayout :: HTMap
controlsLayout = Layout.verticalLayoutHandler 5.0
inLabelsLayout = Layout.verticalLayoutHandler 5.0

expandedGroupStyle, visualizationGroupStyle :: Group.Style
expandedGroupStyle = def & Group.background ?~ Color 0.15 0.15 0.15 0.8
                         & Group.padding .~ uniformPadding 5.0

-- controlsPosition = Vector2 (-30.0) (-30.0)
-- controlsLayout   = Layout.verticalLayoutHandler 5.0
--
-- expandedGroupStyle = def & Group.background ?~ (0.2, 0.2, 0.2)
--                          & Group.padding .~ (Padding 70.0 0.0 10.0 0.0)
--                          & Group.borderRadius .~ (10.0, 10.0, 30.0, 10.0)

expandedGroupLayout = Layout.verticalLayoutHandler 5.0

valueLabel = Label.Label position size align def where
    position = Vector2 (-25.0) 0
    size     = Vector2 100.0 20.0
    align    = Label.Center



visualizationGroupStyle = expandedGroupStyle

plotSize :: Vector2 Double
plotSize = Vector2 200.0 150.0

portControlSize :: Vector2 Double
portControlSize = Vector2 200.0 20.0

setLabelSize, setButtonSize :: Vector2 Double
setLabelSize  = Vector2 (0.7 * (portControlSize ^. x) - setLabelOffsetX) (portControlSize ^. y)
setButtonSize = Vector2 (0.3 * (portControlSize ^. x)) (portControlSize ^. y)

setLabelOffsetX :: Double
setLabelOffsetX = 10.0

labeledPadding :: Padding
labeledPadding = xyPadding setLabelOffsetX 0.0

execTimeLabel :: Text -> Label.Label
execTimeLabel = Label.Label (Vector2 setLabelOffsetX 0) size align def where
    size     = Vector2 0 0 -- (portControlSize & x -~ setLabelOffsetX)
    align    = Label.Left

codeEditorSize :: Vector2 Double
codeEditorSize = Vector2 300.0 150.0
