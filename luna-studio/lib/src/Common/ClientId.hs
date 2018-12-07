{-# LANGUAGE OverloadedStrings #-}

module Common.ClientId (clientId)  where

import           Common.Prelude

import           Data.UUID.Types     (UUID)
import qualified Data.UUID.Types     as UUID

clientId :: UUID
clientId = fromJust . UUID.fromString $ "dee28bb6-1e57-4368-9c68-5752980620fd"
