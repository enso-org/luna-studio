module LunaStudio.API.Persistence.Envelope where

import           Data.Binary                        (Binary)
import           LunaStudio.API.Persistence.Project (Project)
import           Prologue


data Envelope = Envelope { _version :: Int
                         , _project :: Project
                         } deriving (Eq, Generic, NFData, Show)

makeLenses ''Envelope
instance Binary Envelope


pack :: Project -> Envelope
pack = Envelope 1
