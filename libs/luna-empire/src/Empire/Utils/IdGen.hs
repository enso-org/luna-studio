module Empire.Utils.IdGen where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Empire.Prelude

nextId :: IntMap a -> Int
nextId intMap = if IntMap.null intMap then 0 else 1 + (fst . IntMap.findMax $ intMap)
