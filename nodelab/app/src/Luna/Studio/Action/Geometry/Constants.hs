module Luna.Studio.Action.Geometry.Constants
    ( connectionWidth
    , lineHeight
    -- , nodeExpandedWidth
    , nodeRadius
    , nodeRadius'
    , portRadius
    ) where

import           Luna.Studio.Prelude

connectionWidth :: Double
connectionWidth = 2.6

lineHeight :: Double
lineHeight = 16

-- TODO[react]: Unused
nodeExpandedWidth :: Double
nodeExpandedWidth = 270

nodeRadius :: Double
nodeRadius = 20

nodeRadius' :: Double
nodeRadius' = nodeRadius - connectionWidth

portRadius :: Double
portRadius  = nodeRadius - connectionWidth/2
