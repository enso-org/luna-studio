{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Model.App (
    module NodeEditor.React.Model.App,
) where

import           Common.Prelude
import           Data.HashMap.Lazy                  (HashMap)
import           LunaStudio.Data.TypeRep            (TypeRep)
import           NodeEditor.React.Model.Breadcrumbs (Breadcrumbs)
import           NodeEditor.React.Model.NodeEditor  (NodeEditor)


type VisualizatorName = Text
type VisualizatorPath = Text
type Visualizator     = (VisualizatorName, VisualizatorPath)
type VisualizatorsMap = HashMap TypeRep [Visualizator]

data App = App { _breadcrumbs      :: Breadcrumbs
               , _nodeEditor       :: NodeEditor
               , _visualizatorsMap :: VisualizatorsMap
               } deriving (Default, Eq, Generic)

makeLenses ''App
