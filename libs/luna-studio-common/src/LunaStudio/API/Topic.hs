module LunaStudio.API.Topic where

import           Prologue


type Topic = String

class MessageTopic a where
  topic :: Topic

topic' :: forall a. MessageTopic a => a -> Topic
topic' _ = topic @a

request, response, update, typecheck :: Topic
request   = ".request"
response  = ".response"
update    = ".update"
typecheck = ".typecheck"
