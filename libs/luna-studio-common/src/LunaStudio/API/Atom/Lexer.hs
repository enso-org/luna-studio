{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module LunaStudio.API.Atom.Lexer where

import           Data.Binary             (Binary)
import qualified LunaStudio.API.Request  as R
import qualified LunaStudio.API.Response as Response
import qualified LunaStudio.API.Topic    as T
import           Prologue


data Update = Update { _filePath :: FilePath
                     , _tags     :: [(Int, [String])]
                     } deriving (Eq, Generic, NFData, Show)

makeLenses ''Update
instance Binary Update


topicPrefix = "empire.atom.file.lexer"
instance T.MessageTopic Update where topic _ = topicPrefix <> T.update
