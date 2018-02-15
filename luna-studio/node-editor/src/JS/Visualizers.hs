module JS.Visualizers where

import           Common.Prelude             hiding (toList)
import           Control.Arrow              ((&&&))
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.UUID.Types            (UUID)
import           GHCJS.Marshal              (toJSValListOf)
import           GHCJS.Marshal.Pure         (pFromJSVal)
import           JavaScript.Array           (JSArray, toList)
import           LunaStudio.Data.TypeRep    (ConstructorRep)

foreign import javascript safe "Object.keys(typeof window.visualizers == 'object' ? window.visualizers : {})"
    getVisualizers' :: IO JSArray

getVisualizers :: IO [String]
getVisualizers = fmap pFromJSVal . toList <$> getVisualizers'

foreign import javascript safe "(typeof window.internalVisualizers == 'string' ? window.internalVisualizers : '{}')"
    getInternalVisualizers' :: IO JSString

getInternalVisualizers :: IO (Map Text Text)
getInternalVisualizers = fromMaybe def . Aeson.decode . BS.pack . convert <$> getInternalVisualizers'


foreign import javascript safe "window.visualizersPath"
    getVisualizersLibraryPath' :: IO JSString

getVisualizersLibraryPath :: IO FilePath
getVisualizersLibraryPath = convert <$> getVisualizersLibraryPath'


foreign import javascript safe "visualizerFramesManager.sendData($1, $2, $3);"
    sendVisualizationData' :: JSString -> JSString -> JSString -> IO ()

foreign import javascript safe "visualizerFramesManager.sendInternalData($1, $2);"
    sendInternalData' :: JSString -> JSString -> IO ()

foreign import javascript safe "visualizerFramesManager.sendDatapoint($1, $2);"
    sendStreamDatapoint' :: JSString -> JSString -> IO ()

foreign import javascript safe "visualizerFramesManager.register($1);"
    registerVisualizerFrame' :: JSString -> IO ()

foreign import javascript safe "visualizerFramesManager.notifyStreamRestart($1, $2, $3)"
    notifyStreamRestart' :: JSString -> JSString -> JSVal -> IO ()

notifyStreamRestart :: UUID -> ConstructorRep -> [Text] -> IO ()
notifyStreamRestart uid rep backup = notifyStreamRestart' (convert $ show uid) (convert . BS.unpack $ Aeson.encode rep) =<< toJSValListOf backup

sendStreamDatapoint :: UUID -> Text -> IO ()
sendStreamDatapoint uid d = sendStreamDatapoint' (convert $ show uid) (convert d)

registerVisualizerFrame :: UUID -> IO ()
registerVisualizerFrame = registerVisualizerFrame' . convert . show

sendVisualizationData :: UUID -> ConstructorRep -> Text -> IO ()
sendVisualizationData uid rep d = sendVisualizationData' (convert $ show uid) (convert . BS.unpack $ Aeson.encode rep) (convert d)

sendInternalData :: UUID -> Text -> IO ()
sendInternalData uid d = sendInternalData' (convert $ show uid) (convert d)

foreign import javascript safe "window.visualizers[$1]($2)"
    checkVisualizer' :: JSString -> JSString -> IO JSString

checkVisualizer :: String -> String -> IO String
checkVisualizer name rep = convert <$> checkVisualizer' (convert name) (convert rep)

mkVisualizersMap :: IO (Map String (String -> IO String))
mkVisualizersMap = Map.fromList . fmap (id &&& checkVisualizer) <$> getVisualizers
