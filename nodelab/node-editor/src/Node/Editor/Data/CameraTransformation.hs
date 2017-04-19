module Node.Editor.Data.CameraTransformation where

import           Data.Matrix         (Matrix, identity)
import           Luna.Prelude

--TODO[react]: Consider if we can require those Matrices to be squared and of size 4
data CameraTransformation = CameraTransformation { _screenToLogical :: Matrix Double
                                                 , _logicalToScreen :: Matrix Double
                                                 , _lastInverse     :: Int
                                                 } deriving (Show, Eq)

makeLenses ''CameraTransformation

instance Default CameraTransformation where
    def = CameraTransformation (identity 4) (identity 4) 0
