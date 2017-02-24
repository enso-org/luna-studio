module Luna.Studio.Action.Geometry.Constants where

import Luna.Studio.Prelude

gridSize, connectionWidth, lineHeight, nodeExpandedWidth, nodeRadius, nodeRadius', portRadius :: Double

gridSize = 16

connectionWidth   = 2.6
lineHeight        = gridSize
nodeExpandedWidth = 160

nodeRadius  = 20
nodeRadius' = nodeRadius - connectionWidth
portRadius  = nodeRadius - connectionWidth/2
