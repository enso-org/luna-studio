module Style.Node where

import           Luna.Studio.Data.Vector (Position (Position), Size (Size), Vector2 (Vector2), x, y)
import           Luna.Studio.Prelude

import           Style.Types

import qualified Object.Widget.Label     as Label

nodeRadius :: Double
nodeRadius = 25.0

expressionLabel, valueLabel :: Text -> Label.Label
expressionLabel = Label.Label position size align Label.Monospace where
    position = Position (Vector2 (-150.0) (-50.0))
    size     = Size (Vector2 300.0 20.0)
    align    = Label.Center

controlsPosition :: Position
controlsPosition = Position (Vector2 (-nodeRadius) 35.0)

valueLabel = Label.Label position size align def where
    position = Position (Vector2 (-25.0) 0)
    size     = Size (Vector2 100.0 20.0)
    align    = Label.Center

plotSize :: Size
plotSize = Size (Vector2 200.0 150.0)

portControlSize :: Size
portControlSize = Size (Vector2 200.0 20.0)

setLabelSize, setButtonSize :: Size
setLabelSize  = Size (Vector2 (0.7 * (portControlSize ^. x) - setLabelOffsetX) (portControlSize ^. y))
setButtonSize = Size (Vector2 (0.3 * (portControlSize ^. x)) (portControlSize ^. y))

setLabelOffsetX :: Double
setLabelOffsetX = 10.0

labeledPadding :: Padding
labeledPadding = xyPadding setLabelOffsetX 0.0

codeEditorSize :: Size
codeEditorSize = Size (Vector2 300.0 150.0)
