module JS.Visualizers (mkVisualizersMap) where

import           Common.Prelude     hiding (toList)
import           Control.Arrow      ((&&&))
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           GHCJS.Marshal.Pure (pFromJSVal)
import           JavaScript.Array   (JSArray, toList)

foreign import javascript safe "Object.keys(typeof window.visualizers == 'object' ? window.visualizers : {})"
    getVisualizers' :: IO JSArray

getVisualizers :: IO [String]
getVisualizers = fmap pFromJSVal . toList <$> getVisualizers'

foreign import javascript safe "window.visualizers[$1]($2)"
    checkVisualizer' :: JSString -> JSString -> IO JSString

checkVisualizer :: String -> String -> IO String
checkVisualizer name rep = convert <$> checkVisualizer' (convert name) (convert rep)

mkVisualizersMap :: IO (Map String (String -> IO String))
mkVisualizersMap = Map.fromList . fmap (id &&& checkVisualizer) <$> getVisualizers
