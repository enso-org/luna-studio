module LunaStudio.API.Topic where

import           Prologue


type Topic = String

class MessageTopic a where
  topic :: a -> Topic

request, response, update, typecheck :: Topic
request   = ".request"
response  = ".response"
update    = ".update"
typecheck = ".typecheck"
