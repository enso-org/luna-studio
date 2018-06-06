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
import           IdentityString             (IdentityString)
import qualified IdentityString             as IS
import           JavaScript.Array           (JSArray, toList)
import           LunaStudio.Data.TypeRep    (ConstructorRep)


foreign import javascript safe "window.getInternalVisualizersPath()"
    getInternalVisualizersLibraryPath' :: IO JSString

getInternalVisualizersLibraryPath :: IO FilePath
getInternalVisualizersLibraryPath = convert <$> getInternalVisualizersLibraryPath'

foreign import javascript safe "window.getLunaVisualizersPath()"
    getLunaVisualizersLibraryPath' :: IO JSString

getLunaVisualizersLibraryPath :: IO FilePath
getLunaVisualizersLibraryPath = convert <$> getLunaVisualizersLibraryPath'


foreign import javascript safe "res = window.getInternalVisualizers(); $r = Object.keys(typeof res == 'object' ? res : {});"
    getInternalVisualizers' :: IO JSArray

getInternalVisualizers :: IO [String]
getInternalVisualizers = fmap pFromJSVal . toList <$> getInternalVisualizers'

foreign import javascript safe "res = window.getLunaVisualizers(); $r = Object.keys(typeof res == 'object' ? res : {});"
    getLunaVisualizers' :: IO JSArray

getLunaVisualizers :: IO [String]
getLunaVisualizers = fmap pFromJSVal . toList <$> getLunaVisualizers'

foreign import javascript safe "res = window.getProjectVisualizers($1); $r = Object.keys(typeof res == 'object' ? res : {});"
    getProjectVisualizers' :: JSString -> IO JSArray

getProjectVisualizers :: FilePath -> IO [String]
getProjectVisualizers fp = fmap pFromJSVal . toList <$> getProjectVisualizers' (convert fp)

foreign import javascript safe "window.checkInternalVisualizer($1)"
    checkInternalVisualizer' :: JSString -> IO JSString

checkInternalVisualizer :: String -> IO String
checkInternalVisualizer name = convert <$> checkInternalVisualizer' (convert name)

foreign import javascript safe "window.checkLunaVisualizer($1, $2)"
    checkLunaVisualizer' :: JSString -> JSString -> IO JSString

checkLunaVisualizer :: String -> String -> IO String
checkLunaVisualizer name rep = convert <$> checkLunaVisualizer' (convert name) (convert rep)

foreign import javascript safe "window.checkProjectVisualizer($1, $2)"
    checkProjectVisualizer' :: JSString -> JSString -> IO JSString

checkProjectVisualizer :: String -> String -> IO String
checkProjectVisualizer name rep = convert <$> checkProjectVisualizer' (convert name) (convert rep)

mkInternalVisualizersMap :: IO (Map String String)
mkInternalVisualizersMap = getInternalVisualizers >>= fmap Map.fromList . mapM (\name -> (name,) <$> checkInternalVisualizer name)

mkLunaVisualizersMap :: IO (Map String (String -> IO String))
mkLunaVisualizersMap = Map.fromList . fmap (id &&& checkLunaVisualizer) <$> getLunaVisualizers

mkProjectVisualizersMap :: FilePath -> IO (Map String (String -> IO String))
mkProjectVisualizersMap fp = Map.fromList . fmap (id &&& checkProjectVisualizer) <$> getProjectVisualizers fp


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

notifyStreamRestart :: UUID -> ConstructorRep -> [IdentityString] -> IO ()
notifyStreamRestart uid rep backup =
    notifyStreamRestart' (convert $ show uid)
                         (convert . BS.unpack $ Aeson.encode rep)
                         =<< toJSValListOf (view IS.jsString <$> backup)

sendStreamDatapoint :: UUID -> IdentityString -> IO ()
sendStreamDatapoint uid d = sendStreamDatapoint' (convert $ show uid)
                                                 (d ^. IS.jsString)

registerVisualizerFrame :: UUID -> IO ()
registerVisualizerFrame = registerVisualizerFrame' . convert . show

sendVisualizationData :: UUID -> ConstructorRep -> IdentityString -> IO ()
sendVisualizationData uid rep d =
    sendVisualizationData' (convert $ show uid)
                           (convert . BS.unpack $ Aeson.encode rep)
                           (d ^. IS.jsString)

sendInternalData :: UUID -> Text -> IO ()
sendInternalData uid d = sendInternalData' (convert $ show uid) (convert d)
