{-# OPTIONS_GHC -fno-warn-orphans #-}
module GraphDiffSpec (spec) where

import           Control.Lens                         (FunctorWithIndex, imap)
import           Control.Arrow                        ((&&&))
import           Data.Foldable                        (toList)
import           Data.HashMap.Strict                  (HashMap)
import qualified Data.HashMap.Strict                  as HashMap
import qualified Data.Map                             as Map
import qualified Data.Matrix                          as Matrix
import qualified Data.Set                             as Set
import qualified Data.Text                            as Text
import           LunaStudio.Data.Breadcrumb           (Breadcrumb (Breadcrumb), BreadcrumbItem (Arg, Definition, Lambda), Named (Named))
import           LunaStudio.Data.CameraTransformation (CameraTransformation (CameraTransformation))
import           LunaStudio.Data.Code                 (Code (Code))
import           LunaStudio.Data.Connection           (Connection (Connection))
import           LunaStudio.Data.Diff                 (Diff, diff)
import           LunaStudio.Data.Error                (Error (Error), GraphError (..))
import           LunaStudio.Data.Graph                (Graph (Graph))
import           LunaStudio.Data.GUIState             (GUIState (GUIState))
import           LunaStudio.Data.MonadPath            (MonadPath (MonadPath))
import           LunaStudio.Data.Node                 (ExpressionNode (ExpressionNode), InputSidebar (InputSidebar), NodeId,
                                                       OutputSidebar (OutputSidebar))
import qualified LunaStudio.Data.Node                 as Node
import           LunaStudio.Data.NodeMeta             (NodeMeta (..))
import           LunaStudio.Data.Port                 (InPorts (InPorts), LabeledTree (LabeledTree), OutPorts (OutPorts), Port (Port))
import qualified LunaStudio.Data.Port                 as Port
import           LunaStudio.Data.PortDefault          (PortDefault, PortValue)
import qualified LunaStudio.Data.PortDefault          as PortDef
import           LunaStudio.Data.PortRef              (InPortRef (InPortRef), OutPortRef (OutPortRef))
import qualified LunaStudio.Data.PortRef              as PortRef
import qualified LunaStudio.Data.Position             as Position
import           LunaStudio.Data.TypeRep              (TypeRep (TCons, TLam, TStar, TVar))
import           LunaStudio.Data.Visualizer           (Visualizer (Visualizer), VisualizerId (VisualizerId),
                                                       VisualizerType (LunaVisualizer, ProjectVisualizer))
import           Prologue                             hiding (TypeRep, elements, toList)
import           Test.Hspec                           (Spec, describe, it, shouldBe)
import           Test.QuickCheck
import           Test.QuickCheck.Gen                  (chooseAny)


instance Arbitrary Text where
    arbitrary = do
        let allowedChars
                = ['$', '>', '<', '*', '+', '.', '!', '?', '|']
                <> ['0' .. '9']
                <> ['a' .. 'z']
                <> ['A' .. 'Z']
        Text.pack <$> listOf (elements allowedChars)

instance Arbitrary NodeId where
    arbitrary = chooseAny

instance Arbitrary TypeRep where
    arbitrary = do
        let str     = TCons "Text" mempty
            int     = TCons "Text" mempty
            var     = TVar "a"
            listA   = TCons "List" [var]
            intList = TCons "List" [int]
            lam1    = TLam str intList
            lam2    = TLam var int
        oneof $ fmap pure [str, int, listA, intList, var, lam1, lam2, TStar]


instance Arbitrary BreadcrumbItem where
    arbitrary = do
        nid             <- arbitrary
        Positive argNum <- arbitrary
        let defBc = pure $ Definition nid
            lamBc = pure $ Lambda     nid
            argBc = pure $ Arg        nid argNum
        oneof [defBc, lamBc, argBc]

instance (Arbitrary a) => Arbitrary (Named a) where
    arbitrary = do
        name <- arbitrary
        bc   <- arbitrary
        pure $ Named name bc

instance Arbitrary a => Arbitrary (Breadcrumb a) where
    arbitrary = Breadcrumb <$> (choose (0,10) >>= flip vectorOf arbitrary)


instance Arbitrary PortValue where
    arbitrary = oneof [ PortDef.IntValue  <$> arbitrary
                      , PortDef.RealValue <$> arbitrary
                      , PortDef.BoolValue <$> arbitrary
                      , PortDef.TextValue <$> arbitrary
                      ]

instance Arbitrary PortDefault where
    arbitrary = oneof
        [PortDef.Expression <$> arbitrary, PortDef.Constant <$> arbitrary]


instance Arbitrary a => Arbitrary (Port a) where
    arbitrary = do
        pid   <- arbitrary
        name  <- arbitrary
        tpe   <- arbitrary
        state <- oneof [pure Port.NotConnected, Port.WithDefault <$> arbitrary]
        pure $ Port pid name tpe state

genTree
    :: (Mempty (f (LabeledTree f a))
    , Traversable f
    , Arbitrary a
    , Arbitrary (f ())
    ) => Int -> Int -> Gen (LabeledTree f a)
genTree maxDepth maxWidth = go maxDepth where
    go d = LabeledTree <$> subtrees d <*> arbitrary
    subtrees d = do
        depth <- choose (0, d)
        if depth == 0 then pure mempty else do
            (subtreePositions :: f ()) <- resize maxWidth arbitrary
            sequence $ go (depth - 1) <$ subtreePositions

instance Arbitrary a => Arbitrary (InPorts a) where
    arbitrary = InPorts <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (OutPorts a) where
    arbitrary = OutPorts <$> arbitrary

genPortsTree ::  forall f ind.
    (Mempty (f (LabeledTree f (Port ())))
    , Traversable f
    , FunctorWithIndex ind (LabeledTree f)
    , Arbitrary (f ())
    ) => Int -> Int -> Gen (LabeledTree f (Port ind))
genPortsTree maxDepth maxWidth = imap (Port.portId .~)
    <$> (genTree maxDepth maxWidth :: Gen (LabeledTree f (Port ())))

instance Arbitrary NodeMeta where
    arbitrary = do
        pos        <- Position.fromTuple <$> arbitrary
        displayRes <- arbitrary
        vis        <- arbitrary
        pure $ NodeMeta pos displayRes vis

instance Arbitrary ExpressionNode where
    arbitrary = do
        nid      <- arbitrary
        expr     <- arbitrary
        isDef    <- arbitrary
        name     <- arbitrary
        code     <- arbitrary
        inPorts  <- genPortsTree 5 5
        outPorts <- genPortsTree 5 5
        nodeMeta <- arbitrary
        canEnter <- arbitrary
        pure $ ExpressionNode nid expr isDef name code inPorts outPorts
            nodeMeta canEnter

instance Arbitrary InputSidebar where
    arbitrary = InputSidebar <$> arbitrary <*> (choose (0,5)
        >>= \n -> vectorOf n (genPortsTree 5 5))  <*> arbitrary

instance Arbitrary OutputSidebar where
    arbitrary = OutputSidebar <$> arbitrary <*> genPortsTree 5 5

instance Arbitrary Visualizer where
    arbitrary = do
        visName <- arbitrary
        visType <- elements [LunaVisualizer, ProjectVisualizer]
        Visualizer (VisualizerId visName visType) <$> arbitrary

instance Arbitrary (HashMap TypeRep Visualizer) where
    arbitrary = choose (0,20) >>= \n ->
        HashMap.fromList <$> vectorOf n arbitrary

instance Arbitrary CameraTransformation where
    arbitrary = do
        let genMatrix = Matrix.fromList 4 4 <$> vectorOf (4 * 4) arbitrary
        CameraTransformation <$> genMatrix <*> genMatrix <*> arbitrary

genMonadPath :: [NodeId] -> Gen MonadPath
genMonadPath nids = flip MonadPath nids <$> arbitrary

genMonadPaths :: [NodeId] -> Gen [MonadPath]
genMonadPaths []   = pure []
genMonadPaths nids = do
    monadLen <- choose (1, length nids)
    let (monadNids, rest) = splitAt monadLen nids
    (:) <$> genMonadPath monadNids <*> genMonadPaths rest

instance Arbitrary Graph where
    arbitrary = do
        nodes       <- listOf arbitrary
        inSidebar   <- arbitrary
        outSidebar  <- arbitrary
        let portIds :: forall a i f. (Foldable f, FunctorWithIndex i f)
                => f a -> [i]
            portIds = toList . imap const
            inSidebarOutPortRefs :: [OutPortRef]
            inSidebarOutPortRefs
                = fmap (OutPortRef (convert $ inSidebar  ^. Node.inputNodeId))
                . concatMap portIds
                $ inSidebar  ^. Node.inputEdgePorts
            outSidebarInPortRefs :: [InPortRef]
            outSidebarInPortRefs
                = fmap (InPortRef (convert $ outSidebar ^. Node.outputNodeId))
                . portIds $ outSidebar ^. Node.outputEdgePorts
            nodesInPortsRefs :: [InPortRef]
            nodesInPortsRefs = foldl
                (\acc n -> (acc <>)
                    . fmap (InPortRef (convert $ n ^. Node.nodeId)) . portIds
                    $ n ^. Node.inPorts
                )
                mempty
                nodes
            nodesOutPortsRefs :: [OutPortRef]
            nodesOutPortsRefs = foldl
                (\acc n -> (acc <>)
                    . fmap (OutPortRef (convert $ n ^. Node.nodeId)) . portIds
                    $ n ^. Node.outPorts
                )
                mempty
                nodes
            allInPortsRefs  = outSidebarInPortRefs <> nodesInPortsRefs
            allOutPortsRefs = inSidebarOutPortRefs <> nodesOutPortsRefs
        numOfConns <- sized $ \k -> choose (0, min k $ length allInPortsRefs)
        connIds <- if not $ null allOutPortsRefs
            then take numOfConns <$> shuffle allInPortsRefs
            else pure mempty
        let genSrc  dst = suchThat
                (elements allOutPortsRefs)
                (\src -> dst ^. PortRef.nodeLoc /= src ^. PortRef.nodeLoc)
            genConn dst = flip Connection dst <$> genSrc dst
        conns  <- mapM genConn connIds
        monads <- genMonadPaths =<< sublistOf (view Node.nodeId <$> nodes)
        pure $ Graph nodes conns (Just inSidebar) (Just outSidebar) monads

instance Arbitrary (Error GraphError) where
    arbitrary = do
        tpe <- oneof $ pure
            <$> [BreadcrumbDoesNotExist, ParseError, OtherGraphError]
        Error tpe <$> arbitrary

instance Arbitrary Code where
    arbitrary = Code <$> arbitrary

instance Arbitrary GUIState where
    arbitrary = do
        bc          <- arbitrary
        impsLen     <- choose (0, 10)
        imps        <- Set.fromList <$> vectorOf impsLen arbitrary
        defVis      <- arbitrary
        cam         <- arbitrary
        projVisPath <- arbitrary
        code        <- arbitrary
        graph       <- arbitrary
        pure $ GUIState bc imps defVis cam projVisPath code graph

testExactCopies :: GUIState -> Diff
testExactCopies p = diff p p

spec :: Spec
spec = describe "graphDiff" $
    it "Diff of two copies of the same graph is empty" $ property
        $ \p -> testExactCopies p `shouldBe` mempty

