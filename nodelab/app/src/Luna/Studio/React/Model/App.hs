{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module Luna.Studio.React.Model.App (
    module Luna.Studio.React.Model.App,
) where

import           Luna.Studio.React.Model.Breadcrumbs (Breadcrumbs)
import           Luna.Studio.React.Model.CodeEditor  (CodeEditor)
import           Luna.Studio.React.Model.NodeEditor  (NodeEditor)
import           Luna.Studio.React.Store.Ref         (Ref)
import           Luna.Studio.React.Model.Searcher    (Searcher)
import           Luna.Studio.Prelude



data App = App { _breadcrumbs       :: Ref Breadcrumbs
               , _nodeEditor        :: Ref NodeEditor
               , _codeEditor        :: Ref CodeEditor
               , _searcher          :: Ref Searcher
               }

makeLenses ''App
