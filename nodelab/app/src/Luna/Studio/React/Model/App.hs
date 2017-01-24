{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Luna.Studio.React.Model.App (
    module Luna.Studio.React.Model.App,
) where

import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.Breadcrumbs (Breadcrumbs)
import           Luna.Studio.React.Model.CodeEditor  (CodeEditor)
import           Luna.Studio.React.Model.NodeEditor  (NodeEditor)
import           Luna.Studio.React.Model.Searcher    (Searcher)



data App = App { _breadcrumbs       :: Breadcrumbs
               , _nodeEditor        :: NodeEditor
               , _codeEditor        :: CodeEditor
               , _searcher          :: Maybe Searcher
               } deriving (Eq)

makeLenses ''App

instance Default App where
    def = App def def def def
