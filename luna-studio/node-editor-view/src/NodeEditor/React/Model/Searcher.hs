{-# LANGUAGE Strict #-}
module NodeEditor.React.Model.Searcher where

import Common.Prelude

import qualified LunaStudio.Data.NodeLoc                   as NodeLoc
import qualified NodeEditor.React.Model.Searcher.Mode      as Mode
import qualified NodeEditor.React.Model.Searcher.Mode.Node as Node

import LunaStudio.Data.NodeLoc                  (NodeLoc)
import NodeEditor.React.Model.Searcher.Hint     (Hint)
import NodeEditor.React.Model.Searcher.Input    (Input)
import NodeEditor.React.Model.Searcher.Mode     (Mode)
import NodeEditor.React.Model.Searcher.UndoRedo (UndoRedoState)
import NodeEditor.React.Model.Visualization     (RunningVisualization)
import Searcher.Data.Result                     (Result)


----------------------
-- === Searcher === --
----------------------

-- === Definition === --

data Searcher = Searcher
    { _input            :: Input
    , _replaceInput     :: Bool
    , _results          :: [Result Hint]
    , _selectedPosition :: Maybe Int
    , _mode             :: Mode
    , _waiting          :: Bool
    , _undoRedo         :: UndoRedoState
    } deriving (Eq, Generic, Show)

makeLenses ''Searcher

instance NFData Searcher


selectedResult :: Getter Searcher (Maybe (Result Hint))
selectedResult = to $ \s -> let
    mayPosition  = s ^. selectedPosition
    atPosition p = s ^? results . ix p
    in join $! atPosition <$> mayPosition
{-# INLINE selectedResult #-}

inputText :: Getter Searcher Text
inputText = input . to convert
{-# INLINE inputText #-}

documentationVisualization :: Traversal' Searcher RunningVisualization
documentationVisualization = mode . Mode._Node . Node.documentationVisualization . _Just


------------------------
-- === Properties === --
------------------------

-- === Definition === --

data Properties = Properties
    { _searcher              :: Searcher
    , _visualizerLibraryPath :: FilePath
    } deriving (Eq, Generic, Show)

makeLenses ''Properties

instance NFData Properties

-- === API === --

visibleHintsNumber :: Int
visibleHintsNumber = 10
{-# INLINE visibleHintsNumber #-}

mkProperties :: Searcher -> FilePath -> Properties
mkProperties = \s vlp -> let
    selected        = fromMaybe def $! s ^. selectedPosition
    limitResults r  = take visibleHintsNumber $! drop selected r
    visibleSearcher = s & results %~ limitResults
                        & selectedPosition %~ fmap (subtract selected)
    in Properties visibleSearcher vlp
{-# INLINE mkProperties #-}

isRelated :: NodeLoc -> Properties -> Bool
isRelated nl s = let
    nlIdPath = NodeLoc.toNodeIdList nl
    mayNl    = s ^? searcher . mode . Mode._Node . Node.nodeLoc
    sIdPath  = maybe mempty NodeLoc.toNodeIdList mayNl
    in isPrefixOf nlIdPath sIdPath
{-# INLINE isRelated #-}
