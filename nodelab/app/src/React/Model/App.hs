{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Model.App (
    module React.Model.App,
) where

import           React.Model.Breadcrumbs (Breadcrumbs)
import           React.Model.CodeEditor  (CodeEditor)
import           React.Model.NodeEditor  (NodeEditor)
import           React.Store.Ref         (Ref)
import           React.Model.Searcher    (Searcher)
import           Utils.PreludePlus



data App = App { _breadcrumbs       :: Ref Breadcrumbs
               , _nodeEditor        :: Ref NodeEditor
               , _codeEditor        :: Ref CodeEditor
               , _searcher          :: Ref Searcher
               }

makeLenses ''App
