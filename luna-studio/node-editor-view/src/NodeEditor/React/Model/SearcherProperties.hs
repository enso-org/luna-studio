module NodeEditor.React.Model.SearcherProperties
    ( module NodeEditor.React.Model.SearcherProperties
    , Mode (..)
    ) where

import           Common.Prelude
import           LunaStudio.Data.NodeLoc              (NodeLoc)
import qualified LunaStudio.Data.NodeLoc              as NodeLoc
import qualified LunaStudio.Data.PortRef              as PortRef
import           NodeEditor.React.Model.NodeEditor    (VisualizersPaths, lunaVisualizersPath)
import           NodeEditor.React.Model.Searcher      (Match, Mode (..), Searcher, docVisInfo, selectedMatch)
import qualified NodeEditor.React.Model.Searcher      as Searcher
import           NodeEditor.React.Model.Visualization (RunningVisualization)


data SearcherProperties = SearcherProperties { _selected      :: Int
                                             , _selectedMatch :: Maybe Match
                                             , _mode          :: Mode
                                             , _input         :: Text
                                             , _replaceInput  :: Bool
                                             , _visLibPath    :: FilePath
                                             } deriving (Eq, Generic, Show)

makeLenses ''SearcherProperties

toSearcherProperties :: Searcher -> VisualizersPaths -> SearcherProperties
toSearcherProperties s vp = SearcherProperties (s  ^. Searcher.selected)
                                               (s  ^. Searcher.selectedMatch)
                                               (s  ^. Searcher.mode)
                                               (s  ^. Searcher.inputText)
                                               (s  ^. Searcher.replaceInput)
                                               (vp ^. lunaVisualizersPath)

docVis :: Getter SearcherProperties (Maybe RunningVisualization)
docVis = to docVis' where
    docVis' s = case s ^. mode of
      Node _ nmi _ -> nmi ^. docVisInfo
      _            -> Nothing

isSearcherRelated :: NodeLoc -> SearcherProperties -> Bool
isSearcherRelated nl s = isPrefixOf nlIdPath sIdPath where
    nlIdPath = NodeLoc.toNodeIdList nl
    sIdPath = case s ^. mode of
        Node     snl   _ _ -> NodeLoc.toNodeIdList snl
        NodeName snl     _ -> NodeLoc.toNodeIdList snl
        PortName portRef _ -> NodeLoc.toNodeIdList $ portRef ^. PortRef.nodeLoc
        _                  -> []

