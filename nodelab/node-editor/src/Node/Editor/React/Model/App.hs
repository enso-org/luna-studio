{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Node.Editor.React.Model.App (
    module Node.Editor.React.Model.App,
) where

import           Luna.Prelude
import           Node.Editor.React.Model.Breadcrumbs (Breadcrumbs)
import           Node.Editor.React.Model.CodeEditor  (CodeEditor)
import           Node.Editor.React.Model.NodeEditor  (NodeEditor)



data App = App { _breadcrumbs       :: Breadcrumbs
               , _nodeEditor        :: NodeEditor
               , _codeEditor        :: CodeEditor
               } deriving (Default, Eq, Generic)

makeLenses ''App
