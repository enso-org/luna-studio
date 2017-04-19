{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Node.Editor.React.Event.CodeEditor where

import           Data.Aeson          (FromJSON, ToJSON)

import           Luna.Prelude



data Event = ToggleCodeEditor
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
