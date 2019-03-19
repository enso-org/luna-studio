module LunaStudio.API.Topic where

import           Prologue


type Topic = String

-- TODO: Refactor to `Topic` and use always as a qualified import.
class MessageTopic a where
  topic :: Topic

topic' :: forall a. MessageTopic a => a -> Topic
topic' _ = topic @a

request, response, update, typecheck :: Topic
request   = ".request"
response  = ".response"
update    = ".update"
typecheck = ".typecheck"
