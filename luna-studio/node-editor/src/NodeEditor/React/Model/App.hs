{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Model.App (
    module NodeEditor.React.Model.App,
) where

import           Common.Prelude
import           NodeEditor.React.Model.Breadcrumbs (Breadcrumbs)
import           NodeEditor.React.Model.CodeEditor  (CodeEditor)
import           NodeEditor.React.Model.NodeEditor  (NodeEditor)



data App = App { _breadcrumbs       :: Breadcrumbs
               , _nodeEditor        :: NodeEditor
               , _codeEditor        :: CodeEditor
               } deriving (Default, Eq, Generic)

makeLenses ''App
