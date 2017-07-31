{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module LunaStudio.API.Graph.SaveSettings where

import           Control.Lens.Aeson                   (lensJSONParse, lensJSONToEncoding, lensJSONToJSON)
import           Data.Aeson.Types                     (FromJSON (..), ToJSON (..))
import           Data.Binary                          (Binary (..))
import           Data.Hashable                        (Hashable)
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as HashMap
import qualified LunaStudio.API.Graph.Request         as G
import qualified LunaStudio.API.Request               as R
import qualified LunaStudio.API.Response              as Response
import qualified LunaStudio.API.Topic                 as T
import           LunaStudio.Data.CameraTransformation (CameraTransformation)
import           LunaStudio.Data.GraphLocation        (GraphLocation)
import           LunaStudio.Data.NodeValue            (Visualizer)
import           LunaStudio.Data.Project              (LocationSettings)
import           LunaStudio.Data.TypeRep              (TypeRep)
import           Prologue                             hiding (TypeRep)


data Request = Request { _location        :: GraphLocation
                       , _settings        :: LocationSettings
                       } deriving (Eq, Generic, NFData, Show)

makeLenses ''Request

instance Binary Request
instance FromJSON Request where parseJSON = lensJSONParse
instance ToJSON Request where
    toJSON     = lensJSONToJSON
    toEncoding = lensJSONToEncoding
instance G.GraphRequest Request where location = location

type Response = Response.SimpleResponse Request ()
instance Response.ResponseResult Request () ()

topicPrefix :: T.Topic
topicPrefix = "empire.graph.saveSettings"
instance T.MessageTopic (R.Request Request) where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response            where topic _ = topicPrefix <> T.response
