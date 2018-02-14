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
import           LunaStudio.Data.TypeRep    (ConstructorRep, errorTypeRep, toConstructorRep)

foreign import javascript safe "window.updateVisualizers($1)"
    updateVisualizers' :: JSString -> IO ()

foreign import javascript safe "window.updateVisualizers()"
    updateInternalVisualizers' :: IO ()

foreign import javascript safe "Object.keys(typeof window.internalVisualizers == 'object' ? window.internalVisualizers : {})"
    getInternalVisualizers' :: IO JSArray

foreign import javascript safe "Object.keys(typeof window.projectVisualizers == 'object' ? window.projectVisualizers : {})"
    getProjectVisualizers' :: IO JSArray

updateVisualizers :: FilePath -> IO ()
updateVisualizers = updateVisualizers' . convert

updateInternalVisualizers :: IO ()
updateInternalVisualizers = updateInternalVisualizers'

getInternalVisualizers :: IO [String]
getInternalVisualizers = fmap pFromJSVal . toList <$> getInternalVisualizers'

getProjectVisualizers :: IO [String]
getProjectVisualizers = fmap pFromJSVal . toList <$> getProjectVisualizers'

getVisualizers :: IO ([String], [String])
getVisualizers = (,) <$> getInternalVisualizers <*> getProjectVisualizers

foreign import javascript safe "window.internalVisualizersPath"
    getInternalVisualizersLibraryPath' :: IO JSString

getInternalVisualizersLibraryPath :: IO FilePath
getInternalVisualizersLibraryPath = convert <$> getInternalVisualizersLibraryPath'

foreign import javascript safe "window.projectVisualizersPath"
    getProjectVisualizersLibraryPath' :: IO JSVal

getProjectVisualizersLibraryPath :: IO (Maybe FilePath)
getProjectVisualizersLibraryPath = join $ fromJSVal <$> getProjectVisualizersLibraryPath'

getVisualizersLibraryPaths :: IO (FilePath, Maybe FilePath)
getVisualizersLibraryPaths = (,) <$> getInternalVisualizersLibraryPath <*> getProjectVisualizersLibraryPath


foreign import javascript safe "visualizerFramesManager.sendData($1, $2, $3);"
    sendVisualizationData' :: JSString -> JSString -> JSString -> IO ()

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
sendVisualizationData uid rep d' = sendVisualizationData' (convert $ show uid) (convert . BS.unpack $ Aeson.encode rep) (convert d) where
    d = if Just rep == toConstructorRep errorTypeRep then convert . BS.unpack . Aeson.encode $ d' else d'

foreign import javascript safe "window.internalVisualizers[$1]($2)"
    checkInternalVisualizer' :: JSString -> JSString -> IO JSString

checkInternalVisualizer :: String -> String -> IO String
checkInternalVisualizer name rep = convert <$> checkInternalVisualizer' (convert name) (convert rep)

foreign import javascript safe "window.projectVisualizers[$1]($2)"
    checkProjectVisualizer' :: JSString -> JSString -> IO JSString

checkProjectVisualizer :: String -> String -> IO String
checkProjectVisualizer name rep = convert <$> checkProjectVisualizer' (convert name) (convert rep)

mkInternalVisualizersMap :: IO (Map String (String -> IO String))
mkInternalVisualizersMap = Map.fromList . fmap (id &&& checkInternalVisualizer) <$> getInternalVisualizers

mkProjectVisualizersMap :: IO (Map String (String -> IO String))
mkProjectVisualizersMap = Map.fromList . fmap (id &&& checkProjectVisualizer) <$> getProjectVisualizers
