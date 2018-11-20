module NodeEditor.React.Model.Searcher
    ( module NodeEditor.React.Model.Searcher
    , module X
    ) where

import NodeEditor.React.Model.Searcher.Hint  as X
import NodeEditor.React.Model.Searcher.Input as X (Input (DividedInput, RawInput),
                                                   _DividedInput, _RawInput)
import NodeEditor.React.Model.Searcher.Mode  as X hiding (connectedPortRef,
                                                   documentationVisualization)
import Searcher.Engine                       as X (IsMatch, Match, Range,
                                                   SearcherData)

import Common.Prelude

import qualified Data.Text                            as Text
import qualified Data.UUID.Types                      as UUID
import qualified Data.Vector.Unboxed                  as Vector
import qualified Luna.Syntax.Text.Lexer               as Lexer
import qualified LunaStudio.Data.PortRef              as PortRef
import qualified NodeEditor.Event.Shortcut            as Shortcut
import qualified NodeEditor.React.Model.Searcher.Mode as Mode
import qualified Searcher.Engine.Data.Match           as Match

import Data.Text32                          (Text32)
import LunaStudio.Data.NodeLoc              (NodeId, NodeLoc)
import LunaStudio.Data.Port                 (AnyPortId)
import LunaStudio.Data.PortRef              (OutPortRef)
import LunaStudio.Data.Position             (Position)
import NodeEditor.React.Model.Visualization (RunningVisualization)


data Searcher = Searcher
    { _selected      :: Int
    , _searcherMode  :: Mode
    , _input         :: Input
    , _replaceInput  :: Bool
    , _rollbackReady :: Bool
    , _waitingForTc  :: Bool
    , _searcherError :: Maybe Text
    } deriving (Eq, Generic, Show)

makeLenses ''Searcher


class SearcherView s where
    selectedPosition :: Lens' s Int
    mode             :: Lens' s Mode
    inputText        :: Getter s Text
    hints            :: Getter s [Hint]
    hints = to $ getHints . view mode
    selectedHint     :: Getter s (Maybe Hint)
    selectedHint = to $ \s ->
        s ^? hints . ix (s ^. selectedPosition - 1)
    connectedPortRef :: Getter s (Maybe OutPortRef)
    connectedPortRef = to (^. mode . Mode.connectedPortRef)
    documentationVisualization :: Lens' s (Maybe RunningVisualization)
    documentationVisualization = mode . Mode.documentationVisualization

instance SearcherView Searcher where
    selectedPosition = selected
    mode             = searcherMode
    inputText        = to $ convert . view input

getHints :: Mode -> [Hint]
getHints (CommandSearcher m) = CommandHint <$> m
getHints (NodeSearcher    m) = NodeHint    <$> m ^. nodes

getHint :: Int -> Mode -> Maybe Hint
getHint i m = m ^? to getHints . ix i

takeHints :: Int -> Mode -> Mode
takeHints limit (CommandSearcher cs) = CommandSearcher $ take limit cs
takeHints limit (NodeSearcher    ns) = NodeSearcher $ ns & nodes %~ take limit

dropHints :: Int -> Mode -> Mode
dropHints offset (CommandSearcher cs) = CommandSearcher $ drop offset cs
dropHints offset (NodeSearcher    ns) = NodeSearcher $ ns & nodes %~ drop offset
