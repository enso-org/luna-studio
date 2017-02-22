module Luna.Studio.Action.Geometry.Constants
    ( connectionWidth
    , lineHeight
    , nodeExpandedWidth
    , nodeRadius
    , nodeRadius'
    , portRadius
    , grid
    ) where

import           Luna.Studio.Prelude

grid, connectionWidth, lineHeight, nodeExpandedWidth, nodeRadius, nodeRadius', portRadius :: Double

grid = 16

connectionWidth   = 2.6
lineHeight        = grid
nodeExpandedWidth = 160

nodeRadius  = 20
nodeRadius' = nodeRadius - connectionWidth
portRadius  = nodeRadius - connectionWidth/2
