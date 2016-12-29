module Luna.Studio.Data.CoordsTransformation where

import           Data.Matrix         (Matrix, identity)
import           Luna.Studio.Prelude

--TODO[react]: Consider if we can require those Matrices to be squared and of size 4
data CoordsTransformation = CoordsTransformation { _screenToLogical :: Matrix Double
                                                 , _logicalToScreen :: Matrix Double
                                                 }

makeLenses ''CoordsTransformation

instance Default CoordsTransformation where
    def = CoordsTransformation (identity 4) (identity 4)
