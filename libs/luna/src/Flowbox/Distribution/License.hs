---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Distribution.License where

import           Flowbox.Prelude   
import           Flowbox.Data.Version (Version)

import           GHC.Generics      
import           Data.Aeson        

data License = GPL (Maybe Version)
             | AGPL (Maybe Version)
             | LGPL (Maybe Version)
             | BSD3
             | MIT
             | Apache (Maybe Version)
             | PublicDomain
             | AllRightsReserved
             | OtherLicense
             | UnknownLicense String
             deriving (Show, Generic)

-------------------------------------------------
-- INSTANCES
-------------------------------------------------

instance ToJSON License
instance FromJSON License