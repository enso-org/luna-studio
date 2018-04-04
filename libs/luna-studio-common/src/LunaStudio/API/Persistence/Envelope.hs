module LunaStudio.API.Persistence.Envelope where

import           Data.Aeson.Types                   (FromJSON, ToJSON)
import           Data.Binary                        (Binary)
import           LunaStudio.API.Persistence.Project (Project)
import           Prologue


data Envelope = Envelope
    { _version :: Int
    , _project :: Project
    } deriving (Eq, Generic, Show)

makeLenses ''Envelope

instance Binary   Envelope
instance NFData   Envelope
instance FromJSON Envelope
instance ToJSON   Envelope


pack :: Project -> Envelope
pack = Envelope 1
