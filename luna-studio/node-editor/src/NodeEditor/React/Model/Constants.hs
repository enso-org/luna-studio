module NodeEditor.React.Model.Constants
    ( module NodeEditor.React.Model.Constants
    , module X
    ) where

import           Common.Prelude
import           LunaStudio.Data.Constants as X

fontSize, lineHeight, connectionWidth, nodeRadius, nodeRadius', portRadius, argumentConstructorShift, nodeExpandedWidth :: Double

fontSize          = 12
lineHeight        = gridSize

connectionWidth          = 2.2
nodeRadius               = 20
nodeRadius'              = nodeRadius - connectionWidth
portRadius               = nodeRadius - connectionWidth/2
argumentConstructorShift = portRadius

nodeExpandedWidth = X.nodePropertiesWidth
