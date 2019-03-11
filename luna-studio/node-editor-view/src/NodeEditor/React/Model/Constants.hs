module NodeEditor.React.Model.Constants
    ( module NodeEditor.React.Model.Constants
    , module X
    ) where

import           Common.Prelude
import           LunaStudio.Data.Constants as X


fontSize, lineHeight, connectionWidth :: Double
fontSize        = 12
lineHeight      = gridSize
connectionWidth = 2.2

nodeRadius, nodeRadius', portRadius, portAliasRadius :: Double
nodeRadius      = 17
nodeRadius'     = nodeRadius - connectionWidth
portRadius      = nodeRadius - connectionWidth/2
portAliasRadius = 8

nodeExpandedWidth :: Double
nodeExpandedWidth = X.nodePropertiesWidth

searcherWidth, searcherHeight :: Double
searcherWidth  = 460
searcherHeight = 24

selectionPadding, expandedNodePadding :: Double
selectionPadding    = gridSize/2
expandedNodePadding = gridSize/2
