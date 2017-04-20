module JS.UUID (generateUUID) where

import           Common.Prelude

import qualified Data.UUID.Types     as UUID

foreign import javascript safe "generateUUID()" generateUUID' :: IO JSString

generateUUID :: IO UUID.UUID
generateUUID = fromJust . UUID.fromString . convert <$> generateUUID'
