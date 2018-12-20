-- INFO FOR @pmlodawski from dead soldier:
-- The biggest change here is selecetedPosition changed to Maybe Int type.
-- I was just thinking about it logically and this model looks more natural to
-- me and also gets rid of +/-1 when referring to the list or to selectedPosition
-- field. Simply now `selected input = selected Nothing` and that is it. 
-- Also I moved results to top level of searcher, because for some reason we
-- call it searcher, we expect it to search some results. The fact that we are
-- not doing it now for port or node names is because we do not have good hints
-- there yet (or you should consider to introduce new datatype Input and don't
-- run them as searcher).
-- All other changes comes directly from API change or are simple improvements.

{-# LANGUAGE Strict #-}
module NodeEditor.React.Model.Searcher where

import Common.Prelude

import qualified LunaStudio.Data.NodeLoc                   as NodeLoc
import qualified NodeEditor.React.Model.Searcher.Mode      as Mode
import qualified NodeEditor.React.Model.Searcher.Mode.Node as Node

import LunaStudio.Data.NodeLoc               (NodeLoc)
import NodeEditor.React.Model.Searcher.Hint  (Hint)
import NodeEditor.React.Model.Searcher.Input (Input)
import NodeEditor.React.Model.Searcher.Mode  (Mode)
import Searcher.Engine.Data.Result           (Result)



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
    } deriving (Eq, Generic, Show)

makeLenses ''Searcher

instance NFData Searcher


selectedResult :: Getter Searcher (Maybe (Result Hint))
selectedResult = to $ \s -> let
    mayPosition = s ^. selectedPosition
    atPosition  = \p -> s ^? results . ix p
    in join $! atPosition <$> mayPosition
{-# INLINE selectedResult #-}

inputText :: Getter Searcher Text
inputText = input . to convert
{-# INLINE inputText #-}


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
    selected        = fromJust def $! s ^. selectedPosition
    limitResults    = \r -> take visibleHintsNumber $! drop selected r
    visibleSearcher = s & results %~ limitResults
    in Properties visibleSearcher vlp
{-# INLINE mkProperties #-}

isRelated :: NodeLoc -> Properties -> Bool
isRelated nl s = let
    nlIdPath = NodeLoc.toNodeIdList nl
    mayNl    = s ^? searcher . mode . Mode._Node . Node.nodeLoc
    sIdPath  = maybe mempty NodeLoc.toNodeIdList mayNl
    in isPrefixOf nlIdPath sIdPath
{-# INLINE isRelated #-}
