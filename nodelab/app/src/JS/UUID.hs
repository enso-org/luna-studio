module JS.UUID (generateUUID) where

import           Luna.Studio.Prelude

import qualified Data.UUID.Types     as UUID

foreign import javascript safe "generateUUID()" generateUUID' :: IO JSString

generateUUID :: IO UUID.UUID
generateUUID = generateUUID' >>= return . fromJust . UUID.fromString . convert
