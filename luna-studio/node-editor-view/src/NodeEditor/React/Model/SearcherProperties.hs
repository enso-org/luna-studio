module NodeEditor.React.Model.SearcherProperties
    ( module NodeEditor.React.Model.SearcherProperties
    , module X
    ) where

import NodeEditor.React.Model.Searcher as X (Hint, Match, Mode (CommandSearcher, NodeSearcher),
                                             NewNodeData, NodeSearcherData,
                                             NodeSearcherMode (ExpressionMode, NodeNameMode, PortNameMode),
                                             Range, Searcher, SearcherData,
                                             SearcherView, Symbol,
                                             connectionSource, documentation,
                                             documentationVisualization,
                                             dropHints, hints,
                                             matchedCharacters, mode, modeData,
                                             name, newNodeData, nodeLoc, portId,
                                             position, selectedHint,
                                             selectedPosition, takeHints,
                                             _ExpressionMode, _NodeHint,
                                             _NodeNameMode, _NodeSearcher,
                                             _PortNameMode)

import Common.Prelude

import qualified LunaStudio.Data.NodeLoc              as NodeLoc
import qualified LunaStudio.Data.PortRef              as PortRef
import qualified NodeEditor.React.Model.Searcher      as Searcher
import qualified NodeEditor.React.Model.Visualization as Visualization

import LunaStudio.Data.NodeLoc              (NodeLoc)
import NodeEditor.React.Model.Visualization (RunningVisualization, Visualizers)


data SearcherProperties = SearcherProperties
    { _selected              :: Int
    , _searcherMode          :: Mode
    , _input                 :: Text
    , _replaceInput          :: Bool
    , _visualizerLibraryPath :: FilePath
    } deriving (Eq, Generic, Show)

makeLenses ''SearcherProperties

instance SearcherView SearcherProperties where
    selectedPosition = selected
    mode             = searcherMode
    inputText        = input

visibleHintsNumber :: Int
visibleHintsNumber = 10

toSearcherProperties :: Searcher -> Visualizers FilePath -> SearcherProperties
toSearcherProperties s vp =
    let toDrop    = max (s ^. Searcher.selected - 1) 0
    in SearcherProperties
        (s  ^. Searcher.selected)
        (takeHints visibleHintsNumber . dropHints toDrop $ s ^. Searcher.mode)
        (s  ^. Searcher.inputText)
        (s  ^. Searcher.replaceInput)
        (vp ^. Visualization.lunaVisualizers)

isSearcherRelated :: NodeLoc -> SearcherProperties -> Bool
isSearcherRelated nl s = isPrefixOf nlIdPath sIdPath where
    nlIdPath = NodeLoc.toNodeIdList nl
    sIdPath = maybe
        mempty
        NodeLoc.toNodeIdList
        $ s ^? Searcher.mode . Searcher._NodeSearcher . Searcher.nodeLoc
