module NodeEditor.React.Model.Searcher.Mode
    ( module NodeEditor.React.Model.Searcher.Mode
    , module X
    ) where

import NodeEditor.React.Model.Searcher.Mode.Command as X hiding (search)
import NodeEditor.React.Model.Searcher.Mode.Node    as X hiding
                                                          (connectedPortRef,
                                                          documentationVisualization,
                                                          notConnectedEmptyInputSearch,
                                                          search)
import Searcher.Engine                              as X (Match, Symbol)

import Common.Prelude

import qualified NodeEditor.React.Model.Searcher.Mode.Node as NodeSearcher


import LunaStudio.Data.PortRef              (OutPortRef)
import NodeEditor.React.Model.Visualization (RunningVisualization)



data Mode
    = CommandSearcher [Match Command]
    | NodeSearcher    (NodesData Symbol)
    deriving (Eq, Generic, Show)

makePrisms ''Mode

clearHints :: Mode -> Mode
clearHints (CommandSearcher _) = CommandSearcher mempty
clearHints (NodeSearcher   ns) = NodeSearcher $ ns & nodes .~ mempty

connectedPortRef :: Getter Mode (Maybe OutPortRef)
connectedPortRef = to
    $ (^? _NodeSearcher . NodeSearcher.connectedPortRef . _Just)

documentationVisualization :: Lens' Mode (Maybe RunningVisualization)
documentationVisualization =
    let getter m = m ^? _NodeSearcher
            . NodeSearcher.documentationVisualization . _Just
        setter m rv = m & _NodeSearcher
            . NodeSearcher.documentationVisualization .~ rv
    in lens getter setter
