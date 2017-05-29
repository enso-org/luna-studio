module JS.Visualizers (mkVisualizersMap) where

import           Common.Prelude     hiding (toList)
import           JavaScript.Array   (JSArray, toList)
import           GHCJS.Marshal.Pure (pFromJSVal)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Control.Arrow      ((&&&))

foreign import javascript safe "Object.keys(window.visualizers)"
    getVisualizers' :: IO JSArray

getVisualizers :: IO [String]
getVisualizers = fmap pFromJSVal . toList <$> getVisualizers'

foreign import javascript safe "window.visualizers[$1]($2)"
    checkVisualizer' :: JSString -> JSString -> IO JSString

checkVisualizer :: String -> String -> IO String
checkVisualizer name rep = convert <$> checkVisualizer' (convert name) (convert rep)

mkVisualizersMap :: IO (Map String (String -> IO String))
mkVisualizersMap = do
    visualizers <- getVisualizers
    Map.fromList $ (id &&& checkVisualizer) <$> visualizers
