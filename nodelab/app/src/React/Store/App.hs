{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Store.App (
    module React.Store.App,
    module X
) where

import           React.Event.App         as X
import           React.Store.Breadcrumbs (Breadcrumbs)
import           React.Store.CodeEditor  (CodeEditor)
import           React.Store.NodeEditor  (NodeEditor)
import           React.Store.NodeSearcher  (NodeSearcher)
import           React.Store.Ref         (Ref)
import           Utils.PreludePlus



data App = App { _breadcrumbs       :: Ref Breadcrumbs
               , _nodeEditor        :: Ref NodeEditor
               , _codeEditor        :: Ref CodeEditor
               , _codeEditorVisible :: Bool
               , _nodeSearcher      :: Ref NodeSearcher
               }

makeLenses ''App
