module Empire.API.Persistence.Envelope where

import           Prologue

import           Data.Binary                    (Binary)
import           Empire.API.Persistence.Project (Project)

data Envelope = Envelope { _version :: Int
                         , _project :: Project
                         } deriving (Generic, Eq, NFData, Show)

makeLenses ''Envelope

instance Binary Envelope

pack :: Project -> Envelope
pack = Envelope 1
