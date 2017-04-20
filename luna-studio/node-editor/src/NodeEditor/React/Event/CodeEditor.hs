{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Event.CodeEditor where

import           Data.Aeson          (FromJSON, ToJSON)

import           Common.Prelude



data Event = ToggleCodeEditor
            deriving (Show, Generic, NFData, Typeable)

instance ToJSON   Event
instance FromJSON Event
