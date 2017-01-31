module Luna.Studio.Data.Aeson where

import           Data.Aeson          (ToJSON, Value, object, toJSON, (.=))
import           Data.IntMap.Lazy    (IntMap)
import qualified Data.IntMap.Lazy    as IntMap
import qualified Data.Text           as Text
import           Luna.Studio.Prelude hiding ((.=))

intMapToJSON :: ToJSON a => IntMap a -> Value
intMapToJSON intmap = object $ (\(k, v) -> (convert $ show k) .= (toJSON v)) <$> IntMap.toList intmap
