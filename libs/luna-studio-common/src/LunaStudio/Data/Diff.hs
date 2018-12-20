module LunaStudio.Data.Diff where

import Prologue hiding (TypeRep, mod)

import qualified Data.Map                              as Map
import qualified LunaStudio.Data.Graph                 as Graph
import qualified LunaStudio.Data.GUIState              as GUIState
import qualified LunaStudio.Data.Node                  as Node
import qualified LunaStudio.Data.Searcher.Hint.Library as Library

import Control.Lens                         (makePrisms, _Right)
import Data.Aeson.Types                     (ToJSON)
import Data.Binary                          (Binary)
import Data.HashMap.Strict                  (HashMap)
import Data.Map                             (Map)
import Data.Set                             (Set)
import LunaStudio.Data.Breadcrumb           (Breadcrumb, BreadcrumbItem, Named)
import LunaStudio.Data.CameraTransformation (CameraTransformation)
import LunaStudio.Data.Code                 (Code)
import LunaStudio.Data.Connection           (Connection, ConnectionId,
                                             connectionId, toConnectionsMap)
import LunaStudio.Data.Error                (Error, GraphError)
import LunaStudio.Data.Graph                (Graph)
import LunaStudio.Data.GUIState             (GUIState)
import LunaStudio.Data.MonadPath            (MonadPath)
import LunaStudio.Data.Node                 (ExpressionNode, InputSidebar,
                                             OutputSidebar,
                                             toExpressionNodesMap)
import LunaStudio.Data.NodeLoc              (HasNodeLoc (nodeLoc), NodeLoc)
import LunaStudio.Data.NodeMeta             (NodeMeta)
import LunaStudio.Data.Port                 (InPort, InPortTree, OutPort,
                                             OutPortTree)
import LunaStudio.Data.TypeRep              (TypeRep)
import LunaStudio.Data.Visualizer           (ExternalVisualizers, Visualizer)


--TODO: Bump containers to >= 0.5.9 and use merge from Data.Map.Merge.Lazy
mergeMaps :: forall a b c k . Ord k
    => (a -> c) -> (b -> c) -> (a -> b -> c) -> Map k a -> Map k b -> Map k c
mergeMaps whenMissingInSecond whenMissingInFirst whenMatched
    = recursiveMerge mempty where
        recursiveMerge :: Map k c -> Map k a -> Map k b -> Map k c
        recursiveMerge acc m1 m2
            | Map.null m1 && Map.null m2 = acc
            | Map.null m1 = do
                let ((k, v), newM2) = Map.deleteFindMin m2
                    newAcc          = Map.insert k (whenMissingInFirst v) acc
                recursiveMerge newAcc m1 newM2
            | otherwise = do
                let ((k, v), newM1) = Map.deleteFindMin m1
                    resVal          = maybe
                        (whenMissingInSecond v)
                        (whenMatched v)
                        $ Map.lookup k m2
                    newAcc          = Map.insert k resVal acc
                    newM2           = Map.delete k m2
                recursiveMerge newAcc newM1 newM2

-----------------------------------------------------
-- TODO: generate with TH (makeDiffs ''GUIState) vvv


data ModificationAddConnection = ModificationAddConnection
    { _newConnection :: Connection
    } deriving (Eq, Generic, Show)

data ModificationAddNode = ModificationAddNode
    { _newNode :: ExpressionNode
    } deriving (Eq, Generic, Show)

data ModificationRemoveConnection = ModificationRemoveConnection
    { _removeConnectionId :: ConnectionId
    } deriving (Eq, Generic, Show)

data ModificationRemoveNode = ModificationRemoveNode
    { _removeNodeLoc :: NodeLoc
    } deriving (Eq, Generic, Show)

data ModificationRenameNode = ModificationRenameNode
    { _renameNodeLoc :: NodeLoc
    , _newName       :: Maybe Text
    } deriving (Eq, Generic, Show)

data ModificationSetBreadcrumb = ModificationSetBreadcrumb
    { _newBreadcrumb :: Breadcrumb (Named BreadcrumbItem)
    } deriving (Eq, Generic, Show)

data ModificationSetCamera = ModificationSetCamera
    { _newCameraTransformation :: CameraTransformation
    } deriving (Eq, Generic, Show)

data ModificationSetCanEnterNode = ModificationSetCanEnterNode
    { _setCanEnterNodeLoc :: NodeLoc
    , _newCanEnter        :: Bool
    } deriving (Eq, Generic, Show)

data ModificationSetCode = ModificationSetCode
    { _newCode :: Code
    } deriving (Eq, Generic, Show)

data ModificationSetDefaultVisualizers = ModificationSetDefaultVisualizers
    { _newDefaultVisualizers :: HashMap TypeRep Visualizer
    } deriving (Eq, Generic, Show)

data ModificationSetExpression = ModificationSetExpression
    { _setExpressionNodeLoc :: NodeLoc
    , _newExpression        :: Text
    } deriving (Eq, Generic, Show)

data ModificationSetGraph = ModificationSetGraph
    { _newGraph :: Graph
    } deriving (Eq, Generic, Show)

data ModificationSetGraphError = ModificationSetGraphError
    { _graphError :: Error GraphError
    } deriving (Eq, Generic, Show)

data ModificationSetImports = ModificationSetImports
    { _newImports :: Set Library.Name
    } deriving (Eq, Generic, Show)

data ModificationSetInPorts = ModificationSetInPorts
    { _setInPortsNodeLoc :: NodeLoc
    , _newInPortTree     :: InPortTree InPort
    } deriving (Eq, Generic, Show)

data ModificationSetInputSidebar = ModificationSetInputSidebar
    { _newInputSidebar :: Maybe InputSidebar
    } deriving (Eq, Generic, Show)

data ModificationSetIsDefinition = ModificationSetIsDefinition
    { _setIsDefinitionNodeLoc :: NodeLoc
    , _newIsDefinition        :: Bool
    } deriving (Eq, Generic, Show)

data ModificationSetMonadPath = ModificationSetMonadPath
    { _newMonadPath :: [MonadPath]
    } deriving (Eq, Generic, Show)

data ModificationSetNodeCode = ModificationSetNodeCode
    { _setNodeCodeNodeLoc :: NodeLoc
    , _newNodeCode        :: Text
    } deriving (Eq, Generic, Show)

data ModificationSetNodeMeta = ModificationSetNodeMeta
    { _setNodeMetaNodeLoc :: NodeLoc
    , _newNodeMeta        :: NodeMeta
    } deriving (Eq, Generic, Show)

data ModificationSetOutPorts = ModificationSetOutPorts
    { _setOutPortsNodeLoc :: NodeLoc
    , _newOutPortTree     :: OutPortTree OutPort
    } deriving (Eq, Generic, Show)

data ModificationSetOutputSidebar = ModificationSetOutputSidebar
    { _newOutputSidebar :: Maybe OutputSidebar
    } deriving (Eq, Generic, Show)

data ModificationSetExternalVisPath = ModificationSetExternalVisPath
    { _newExternalVisPath :: ExternalVisualizers FilePath
    } deriving (Eq, Generic, Show)

makeLenses ''ModificationAddConnection
makeLenses ''ModificationAddNode
makeLenses ''ModificationRemoveConnection
makeLenses ''ModificationRemoveNode
makeLenses ''ModificationRenameNode
makeLenses ''ModificationSetBreadcrumb
makeLenses ''ModificationSetCamera
makeLenses ''ModificationSetCanEnterNode
makeLenses ''ModificationSetCode
makeLenses ''ModificationSetDefaultVisualizers
makeLenses ''ModificationSetExpression
makeLenses ''ModificationSetGraph
makeLenses ''ModificationSetGraphError
makeLenses ''ModificationSetImports
makeLenses ''ModificationSetInPorts
makeLenses ''ModificationSetInputSidebar
makeLenses ''ModificationSetIsDefinition
makeLenses ''ModificationSetMonadPath
makeLenses ''ModificationSetNodeCode
makeLenses ''ModificationSetNodeMeta
makeLenses ''ModificationSetOutPorts
makeLenses ''ModificationSetOutputSidebar
makeLenses ''ModificationSetExternalVisPath

instance Binary ModificationAddConnection
instance NFData ModificationAddConnection
instance ToJSON ModificationAddConnection
instance Binary ModificationAddNode
instance NFData ModificationAddNode
instance ToJSON ModificationAddNode
instance Binary ModificationRemoveConnection
instance NFData ModificationRemoveConnection
instance ToJSON ModificationRemoveConnection
instance Binary ModificationRemoveNode
instance NFData ModificationRemoveNode
instance ToJSON ModificationRemoveNode
instance Binary ModificationRenameNode
instance NFData ModificationRenameNode
instance ToJSON ModificationRenameNode
instance Binary ModificationSetBreadcrumb
instance NFData ModificationSetBreadcrumb
instance ToJSON ModificationSetBreadcrumb
instance Binary ModificationSetCamera
instance NFData ModificationSetCamera
instance ToJSON ModificationSetCamera
instance Binary ModificationSetCanEnterNode
instance NFData ModificationSetCanEnterNode
instance ToJSON ModificationSetCanEnterNode
instance Binary ModificationSetCode
instance NFData ModificationSetCode
instance ToJSON ModificationSetCode
instance Binary ModificationSetDefaultVisualizers
instance NFData ModificationSetDefaultVisualizers
instance ToJSON ModificationSetDefaultVisualizers
instance Binary ModificationSetExpression
instance NFData ModificationSetExpression
instance ToJSON ModificationSetExpression
instance Binary ModificationSetGraph
instance NFData ModificationSetGraph
instance ToJSON ModificationSetGraph
instance Binary ModificationSetGraphError
instance NFData ModificationSetGraphError
instance ToJSON ModificationSetGraphError
instance Binary ModificationSetImports
instance NFData ModificationSetImports
instance ToJSON ModificationSetImports
instance Binary ModificationSetInPorts
instance NFData ModificationSetInPorts
instance ToJSON ModificationSetInPorts
instance Binary ModificationSetInputSidebar
instance NFData ModificationSetInputSidebar
instance ToJSON ModificationSetInputSidebar
instance Binary ModificationSetIsDefinition
instance NFData ModificationSetIsDefinition
instance ToJSON ModificationSetIsDefinition
instance Binary ModificationSetMonadPath
instance NFData ModificationSetMonadPath
instance ToJSON ModificationSetMonadPath
instance Binary ModificationSetNodeCode
instance NFData ModificationSetNodeCode
instance ToJSON ModificationSetNodeCode
instance Binary ModificationSetNodeMeta
instance NFData ModificationSetNodeMeta
instance ToJSON ModificationSetNodeMeta
instance Binary ModificationSetOutPorts
instance NFData ModificationSetOutPorts
instance ToJSON ModificationSetOutPorts
instance Binary ModificationSetOutputSidebar
instance NFData ModificationSetOutputSidebar
instance ToJSON ModificationSetOutputSidebar
instance Binary ModificationSetExternalVisPath
instance NFData ModificationSetExternalVisPath
instance ToJSON ModificationSetExternalVisPath


data Modification
    = AddConnection         ModificationAddConnection
    | AddNode               ModificationAddNode
    | RemoveConnection      ModificationRemoveConnection
    | RemoveNode            ModificationRemoveNode
    | RenameNode            ModificationRenameNode
    | SetBreadcrumb         ModificationSetBreadcrumb
    | SetCamera             ModificationSetCamera
    | SetCanEnterNode       ModificationSetCanEnterNode
    | SetCode               ModificationSetCode
    | SetDefaultVisualizers ModificationSetDefaultVisualizers
    | SetExpression         ModificationSetExpression
    | SetGraph              ModificationSetGraph
    | SetGraphError         ModificationSetGraphError
    | SetImports            ModificationSetImports
    | SetInPorts            ModificationSetInPorts
    | SetInputSidebar       ModificationSetInputSidebar
    | SetIsDefinition       ModificationSetIsDefinition
    | SetMonadPath          ModificationSetMonadPath
    | SetNodeCode           ModificationSetNodeCode
    | SetNodeMeta           ModificationSetNodeMeta
    | SetOutPorts           ModificationSetOutPorts
    | SetOutputSidebar      ModificationSetOutputSidebar
    | SetExternalVisPath    ModificationSetExternalVisPath
    deriving (Eq, Generic, Show)

makePrisms ''Modification

instance Binary Modification
instance NFData Modification
instance ToJSON Modification


-- TH ^^^
-----------------------------------------------------

class IsModification a where
    toModification :: a -> Modification
    toDiff :: a -> Diff
    toDiff = Diff . pure . toModification


-----------------------------------------------------
-- TODO: generate with TH (makeDiffs ''GUIState) vvv

instance IsModification ModificationAddConnection         where
    toModification = AddConnection
instance IsModification ModificationAddNode               where
    toModification = AddNode
instance IsModification ModificationRemoveConnection      where
    toModification = RemoveConnection
instance IsModification ModificationRemoveNode            where
    toModification = RemoveNode
instance IsModification ModificationRenameNode            where
    toModification = RenameNode
instance IsModification ModificationSetBreadcrumb         where
    toModification = SetBreadcrumb
instance IsModification ModificationSetCamera             where
    toModification = SetCamera
instance IsModification ModificationSetCanEnterNode       where
    toModification = SetCanEnterNode
instance IsModification ModificationSetCode               where
    toModification = SetCode
instance IsModification ModificationSetDefaultVisualizers where
    toModification = SetDefaultVisualizers
instance IsModification ModificationSetExpression         where
    toModification = SetExpression
instance IsModification ModificationSetGraph              where
    toModification = SetGraph
instance IsModification ModificationSetGraphError         where
    toModification = SetGraphError
instance IsModification ModificationSetImports            where
    toModification = SetImports
instance IsModification ModificationSetInPorts            where
    toModification = SetInPorts
instance IsModification ModificationSetInputSidebar       where
    toModification = SetInputSidebar
instance IsModification ModificationSetIsDefinition       where
    toModification = SetIsDefinition
instance IsModification ModificationSetMonadPath          where
    toModification = SetMonadPath
instance IsModification ModificationSetNodeCode           where
    toModification = SetNodeCode
instance IsModification ModificationSetNodeMeta           where
    toModification = SetNodeMeta
instance IsModification ModificationSetOutPorts           where
    toModification = SetOutPorts
instance IsModification ModificationSetOutputSidebar      where
    toModification = SetOutputSidebar
instance IsModification ModificationSetExternalVisPath    where
    toModification = SetExternalVisPath


instance HasNodeLoc ModificationRemoveNode      where
    nodeLoc = removeNodeLoc
instance HasNodeLoc ModificationRenameNode      where
    nodeLoc = renameNodeLoc
instance HasNodeLoc ModificationSetCanEnterNode where
    nodeLoc = setCanEnterNodeLoc
instance HasNodeLoc ModificationSetExpression   where
    nodeLoc = setExpressionNodeLoc
instance HasNodeLoc ModificationSetInPorts      where
    nodeLoc = setInPortsNodeLoc
instance HasNodeLoc ModificationSetIsDefinition where
    nodeLoc = setIsDefinitionNodeLoc
instance HasNodeLoc ModificationSetNodeCode     where
    nodeLoc = setNodeCodeNodeLoc
instance HasNodeLoc ModificationSetNodeMeta     where
    nodeLoc = setNodeMetaNodeLoc
instance HasNodeLoc ModificationSetOutPorts     where
    nodeLoc = setOutPortsNodeLoc

-- TODO: with Wojtek
getNodeModificationNodeLoc :: Modification -> Maybe NodeLoc
getNodeModificationNodeLoc (RenameNode      m) = Just $ m ^. nodeLoc
getNodeModificationNodeLoc (SetCanEnterNode m) = Just $ m ^. nodeLoc
getNodeModificationNodeLoc (SetExpression   m) = Just $ m ^. nodeLoc
getNodeModificationNodeLoc (SetInPorts      m) = Just $ m ^. nodeLoc
getNodeModificationNodeLoc (SetIsDefinition m) = Just $ m ^. nodeLoc
getNodeModificationNodeLoc (SetNodeCode     m) = Just $ m ^. nodeLoc
getNodeModificationNodeLoc (SetNodeMeta     m) = Just $ m ^. nodeLoc
getNodeModificationNodeLoc (SetOutPorts     m) = Just $ m ^. nodeLoc
getNodeModificationNodeLoc _                   = Nothing

-- TH ^^^
-----------------------------------------------------




data Diff = Diff
    { _reversedModifications :: [Modification]
    } deriving (Eq, Generic, Show)

makeLenses ''Diff

instance Binary    Diff
instance NFData    Diff
instance ToJSON    Diff
instance Mempty    Diff where mempty = Diff mempty
instance Semigroup Diff where
    (Diff reversed1) <> (Diff reversed2) = Diff $ reversed2 <> reversed1

class Diffable a where
    patch   :: Modification -> (a -> a)
    diff    :: a -> a -> Diff
    patches :: [Modification] -> (a -> a)
    patches mods v = foldl' (flip patch) v mods
    apply   :: Diff -> (a -> a)
    apply (Diff mods) v = flip patches v $ reverse mods

-----------------------------------------------------
-- TODO: generate with TH (makeDiffs ''GUIState) vvv

instance Diffable ExpressionNode where
    patch (RenameNode      m) n = n & Node.name         .~ m ^. newName
    patch (SetCanEnterNode m) n = n & Node.canEnter     .~ m ^. newCanEnter
    patch (SetExpression   m) n = n & Node.expression   .~ m ^. newExpression
    patch (SetInPorts      m) n = n & Node.inPorts      .~ m ^. newInPortTree
    patch (SetIsDefinition m) n = n & Node.isDefinition .~ m ^. newIsDefinition
    patch (SetNodeCode     m) n = n & Node.code         .~ m ^. newNodeCode
    patch (SetNodeMeta     m) n = n & Node.nodeMeta     .~ m ^. newNodeMeta
    patch (SetOutPorts     m) n = n & Node.outPorts     .~ m ^. newOutPortTree
    patch _                   n = n
    diff n1 n2 = Diff mods where
        nl   = convert $ n2 ^. Node.nodeId
        mods = catMaybes
            [ justIf (n1 ^. Node.code         /= n2 ^. Node.code)         $ toModification . ModificationSetExpression   nl $ n2 ^. Node.expression
            , justIf (n1 ^. Node.isDefinition /= n2 ^. Node.isDefinition) $ toModification . ModificationSetIsDefinition nl $ n2 ^. Node.isDefinition
            , justIf (n1 ^. Node.name         /= n2 ^. Node.name)         $ toModification . ModificationRenameNode      nl $ n2 ^. Node.name
            , justIf (n1 ^. Node.code         /= n2 ^. Node.code)         $ toModification . ModificationSetNodeCode     nl $ n2 ^. Node.code
            , justIf (n1 ^. Node.inPorts      /= n2 ^. Node.inPorts)      $ toModification . ModificationSetInPorts      nl $ n2 ^. Node.inPorts
            , justIf (n1 ^. Node.outPorts     /= n2 ^. Node.outPorts)     $ toModification . ModificationSetOutPorts     nl $ n2 ^. Node.outPorts
            , justIf (n1 ^. Node.nodeMeta     /= n2 ^. Node.nodeMeta)     $ toModification . ModificationSetNodeMeta     nl $ n2 ^. Node.nodeMeta
            , justIf (n1 ^. Node.canEnter     /= n2 ^. Node.canEnter)     $ toModification . ModificationSetCanEnterNode nl $ n2 ^. Node.canEnter
            ]

instance Diffable (Map NodeLoc ExpressionNode) where
    patch (AddNode         m) nodes = Map.insert (convert $ m ^. newNode . Node.nodeId) (m ^. newNode) nodes
    patch (RemoveNode      m) nodes = Map.delete (m ^. removeNodeLoc) nodes
    patch m                   nodes = maybe nodes (\nl -> Map.update (Just . patch m) nl nodes) $ getNodeModificationNodeLoc m
    diff m1 m2 = foldl' (<>) mempty . Map.elems $ mergeMaps nodeWhenMissingIn2 nodeWhenMissingIn1 diff m1 m2 where
        nodeWhenMissingIn2 n = Diff . pure . toModification . ModificationRemoveNode $ convert $ n ^. Node.nodeId
        nodeWhenMissingIn1 n = Diff . pure . toModification $ ModificationAddNode n

instance Diffable [ExpressionNode] where
    patch m     = Map.elems . patch m . toExpressionNodesMap
    diff  n1 n2 = diff (toExpressionNodesMap n1) (toExpressionNodesMap n2)
    apply d     = Map.elems . apply d . toExpressionNodesMap

instance Diffable (Map ConnectionId Connection) where
    patch (AddConnection    m) = let c = m ^. newConnection in Map.insert (c ^. connectionId) c
    patch (RemoveConnection m) = Map.delete (m ^. removeConnectionId)
    patch _                    = id
    diff cs1 cs2 = foldl' (<>) mempty . Map.elems $ mergeMaps connWhenMissingIn2 connWhenMissingIn1 connWhenMatched cs1 cs2 where
        connWhenMissingIn2 c  = Diff . pure . toModification . ModificationRemoveConnection $ c ^. connectionId
        connWhenMissingIn1 c  = Diff . pure . toModification $ ModificationAddConnection c
        connWhenMatched c1 c2 = if c1 == c2 then mempty else Diff . pure . toModification $ ModificationAddConnection c2

instance Diffable [Connection] where
    patch m    = Map.elems . patch m . toConnectionsMap
    diff c1 c2 = diff (toConnectionsMap c1) (toConnectionsMap c2)
    apply d    = Map.elems . apply d . toConnectionsMap

instance Diffable Graph where
    patch mod@(AddConnection    _) g = g & Graph.connections   %~ patch mod
    patch mod@(AddNode          _) g = g & Graph.nodes         %~ patch mod
    patch mod@(RemoveConnection _) g = g & Graph.connections   %~ patch mod
    patch mod@(RemoveNode       _) g = g & Graph.nodes         %~ patch mod
    patch mod@(RenameNode       _) g = g & Graph.nodes         %~ patch mod
    patch mod@(SetCanEnterNode  _) g = g & Graph.nodes         %~ patch mod
    patch mod@(SetExpression    _) g = g & Graph.nodes         %~ patch mod
    patch mod@(SetImports       _) g = g & Graph.imports       %~ patch mod
    patch mod@(SetInPorts       _) g = g & Graph.nodes         %~ patch mod
    patch     (SetInputSidebar  m) g = g & Graph.inputSidebar  .~ m ^. newInputSidebar
    patch mod@(SetIsDefinition  _) g = g & Graph.nodes         %~ patch mod
    patch     (SetMonadPath     m) g = g & Graph.monads        .~ m ^. newMonadPath
    patch mod@(SetNodeCode      _) g = g & Graph.nodes         %~ patch mod
    patch mod@(SetNodeMeta      _) g = g & Graph.nodes         %~ patch mod
    patch mod@(SetOutPorts      _) g = g & Graph.nodes         %~ patch mod
    patch     (SetOutputSidebar m) g = g & Graph.outputSidebar .~ m ^. newOutputSidebar
    patch     _                    g = g
    diff g1 g2 = nodesDiff <> inSidebarDiff <> outSidebarDiff <> connsDiff <> monadsDiff <> importsDiff where
        nodesDiff     = diff (g1 ^. Graph.nodes) (g2 ^. Graph.nodes)
        connsDiff     = diff (g1 ^. Graph.connections) (g2 ^. Graph.connections)
        inSidebarDiff = Diff $ if g1 ^. Graph.inputSidebar /= g2 ^. Graph.inputSidebar
            then pure . toModification . ModificationSetInputSidebar $ g2 ^. Graph.inputSidebar
            else mempty
        outSidebarDiff = Diff $ if g1 ^. Graph.outputSidebar /= g2 ^. Graph.outputSidebar
            then pure . toModification . ModificationSetOutputSidebar $ g2 ^. Graph.outputSidebar
            else mempty
        monadsDiff     = Diff $ if g1 ^. Graph.monads /= g2 ^. Graph.monads
            then pure . toModification . ModificationSetMonadPath $ g2 ^. Graph.monads
            else mempty
        importsDiff    = Diff $ if g1 ^. Graph.imports /= g2 ^. Graph.imports
            then pure . toModification . ModificationSetImports $ g2 ^. Graph.imports
            else mempty

instance Diffable (Either (Error GraphError) Graph) where
    patch (SetGraphError m) _         = Left  $ m ^. graphError
    patch (SetGraph      m) _         = Right $ m ^. newGraph
    patch _                 (Left  e) = Left e
    patch mod               (Right g) = Right $ patch mod g
    diff _          (Left  e)  = Diff . pure . toModification $ ModificationSetGraphError e
    diff (Left _)   (Right g)  = Diff . pure . toModification $ ModificationSetGraph      g
    diff (Right g1) (Right g2) = diff g1 g2

instance Diffable (Set Library.Name) where
    patch (SetImports m) = const $ m ^. newImports
    patch _              = id
    diff imps1 imps2 = if imps1 == imps2
        then mempty
        else Diff . pure . toModification $ ModificationSetImports imps2


instance Diffable (Breadcrumb (Named BreadcrumbItem)) where
    patch (SetBreadcrumb m) = const $ m ^. newBreadcrumb
    patch _                 = id
    diff bc1 bc2 = if bc1 == bc2
        then mempty
        else Diff . pure . toModification $ ModificationSetBreadcrumb bc2

instance Diffable (HashMap TypeRep Visualizer) where
    patch (SetDefaultVisualizers m) = const $ m ^. newDefaultVisualizers
    patch _                         = id
    diff defVis1 defVis2 = if defVis1 == defVis2
        then mempty
        else Diff . pure . toModification $ ModificationSetDefaultVisualizers defVis2

instance Diffable CameraTransformation where
    patch (SetCamera m) = const $ m ^. newCameraTransformation
    patch _             = id
    diff cam1 cam2 = if cam1 == cam2
        then mempty
        else Diff . pure . toModification $ ModificationSetCamera cam2


instance Diffable (ExternalVisualizers FilePath) where
    patch (SetExternalVisPath m) = const $ m ^. newExternalVisPath
    patch _                      = id
    diff p1 p2 = if p1 == p2
        then mempty
        else Diff . pure . toModification $ ModificationSetExternalVisPath p2

instance Diffable Code where
    patch (SetCode m) = const $ m ^. newCode
    patch _           = id
    diff c1 c2 = if c1 == c2
        then mempty
        else Diff . pure . toModification $ ModificationSetCode c2

instance Diffable GUIState where
    patch mod@(AddConnection         _) s = s & GUIState.graph . _Right           %~ patch mod
    patch mod@(AddNode               _) s = s & GUIState.graph . _Right           %~ patch mod
    patch mod@(RemoveConnection      _) s = s & GUIState.graph . _Right           %~ patch mod
    patch mod@(RemoveNode            _) s = s & GUIState.graph . _Right           %~ patch mod
    patch mod@(RenameNode            _) s = s & GUIState.graph . _Right           %~ patch mod
    patch     (SetBreadcrumb         m) s = s & GUIState.breadcrumb               .~ m ^. newBreadcrumb
    patch     (SetCamera             m) s = s & GUIState.camera                   .~ m ^. newCameraTransformation
    patch mod@(SetCanEnterNode       _) s = s & GUIState.graph . _Right           %~ patch mod
    patch     (SetCode               m) s = s & GUIState.code                     .~ m ^. newCode
    patch     (SetDefaultVisualizers m) s = s & GUIState.defaultVisualizers       .~ m ^. newDefaultVisualizers
    patch mod@(SetExpression         _) s = s & GUIState.graph . _Right           %~ patch mod
    patch     (SetGraph              m) s = s & GUIState.graph                    .~ Right (m ^. newGraph)
    patch     (SetGraphError         m) s = s & GUIState.graph                    .~ Left  (m ^. graphError)
    patch mod@(SetImports            _) s = s & GUIState.graph . _Right           %~ patch mod
    patch mod@(SetInPorts            _) s = s & GUIState.graph . _Right           %~ patch mod
    patch mod@(SetInputSidebar       _) s = s & GUIState.graph . _Right           %~ patch mod
    patch mod@(SetIsDefinition       _) s = s & GUIState.graph . _Right           %~ patch mod
    patch mod@(SetMonadPath          _) s = s & GUIState.graph . _Right           %~ patch mod
    patch mod@(SetNodeCode           _) s = s & GUIState.graph . _Right           %~ patch mod
    patch mod@(SetNodeMeta           _) s = s & GUIState.graph . _Right           %~ patch mod
    patch mod@(SetOutPorts           _) s = s & GUIState.graph . _Right           %~ patch mod
    patch mod@(SetOutputSidebar      _) s = s & GUIState.graph . _Right           %~ patch mod
    patch     (SetExternalVisPath    m) s = s & GUIState.externalVisualizersPaths .~ m ^. newExternalVisPath
    diff s1 s2 = result where
        graphDiff = if s1 ^. GUIState.graph == s2 ^. GUIState.graph
            then mempty
            else case (s1 ^. GUIState.graph, s2 ^. GUIState.graph) of
                (_       , Left  e ) -> Diff . pure . toModification $ ModificationSetGraphError e
                (Left  _ , Right g ) -> Diff . pure . toModification $ ModificationSetGraph      g
                (Right g1, Right g2) -> diff g1 g2
        result = diff (s1 ^. GUIState.breadcrumb)               (s2 ^. GUIState.breadcrumb)
              <> diff (s1 ^. GUIState.defaultVisualizers)       (s2 ^. GUIState.defaultVisualizers)
              <> diff (s1 ^. GUIState.camera)                   (s2 ^. GUIState.camera)
              <> diff (s1 ^. GUIState.externalVisualizersPaths) (s2 ^. GUIState.externalVisualizersPaths)
              <> diff (s1 ^. GUIState.code)                     (s2 ^. GUIState.code)
              <> graphDiff

-- TH ^^^
-----------------------------------------------------


guiStateDiff :: GUIState -> Diff
guiStateDiff s = mconcat [bcDiff, defVisDiff, camDiff, visPathDiff, codeDiff,
    graphDiff] where
        bcDiff      = toDiff . ModificationSetBreadcrumb
                    $ s ^. GUIState.breadcrumb
        defVisDiff  = toDiff . ModificationSetDefaultVisualizers
                    $ s ^. GUIState.defaultVisualizers
        camDiff     = toDiff . ModificationSetCamera
                    $ s ^. GUIState.camera
        visPathDiff = toDiff . ModificationSetExternalVisPath
                    $ s ^. GUIState.externalVisualizersPaths
        codeDiff    = toDiff . ModificationSetCode
                    $ s ^. GUIState.code
        graphDiff   = case s ^. GUIState.graph of
            Left  e -> toDiff $ ModificationSetGraphError e
            Right g -> toDiff $ ModificationSetGraph      g
