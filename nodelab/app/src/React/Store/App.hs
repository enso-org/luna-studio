{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Store.App where

import           Control.DeepSeq        (NFData)
import           Utils.PreludePlus

import           React.Store.CodeEditor (CodeEditor)
import           React.Store.NodeEditor (NodeEditor)
import           React.Store.Ref        (Ref)



data App = App { _nodeEditor        :: Ref NodeEditor
               , _codeEditor        :: Ref CodeEditor
               , _codeEditorVisible :: Bool
               }

makeLenses ''App

data Action = Action
            deriving (Show, Generic, NFData, Typeable)
