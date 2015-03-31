---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Luna.Pass.Transform.Graph.Builder.Builder where

import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified Data.Maybe                 as Maybe

import           Flowbox.Prelude                          hiding (Traversal, error, mapM, mapM_)
import           Flowbox.System.Log.Logger
import           Luna.Data.StructInfo                     (StructInfo)
import           Luna.Pass.Transform.Graph.Builder.ArgRef (ArgRef)
import qualified Luna.Pass.Transform.Graph.Builder.ArgRef as ArgRef
import           Luna.Pass.Transform.Graph.Builder.State  (GBPass)
import qualified Luna.Pass.Transform.Graph.Builder.State  as State
import           Luna.Syntax.Arg                          (Arg (Arg))
import qualified Luna.Syntax.Decl                         as Decl
import           Luna.Syntax.Enum                         (Enumerated)
import qualified Luna.Syntax.Enum                         as Enum
import           Luna.Syntax.Expr                         (LExpr)
import qualified Luna.Syntax.Expr                         as Expr
import qualified Luna.Syntax.Graph.DefaultsMap            as DefaultsMap
import           Luna.Syntax.Graph.Graph                  (Graph)
import qualified Luna.Syntax.Graph.Node                   as Node
import           Luna.Syntax.Graph.Node.Expr              (NodeExpr)
import qualified Luna.Syntax.Graph.Node.Expr              as NodeExpr
import qualified Luna.Syntax.Graph.Node.MultiPart         as MultiPart
import qualified Luna.Syntax.Graph.Node.StringExpr        as StringExpr
import           Luna.Syntax.Graph.Port                   (DstPort, SrcPort)
import qualified Luna.Syntax.Graph.Port                   as Port
import           Luna.Syntax.Graph.Tag                    (TDecl, TExpr, TPat, Tag)
import           Luna.Syntax.Label                        (Label (Label))
import qualified Luna.Syntax.Label                        as Label
import           Luna.Syntax.Lit                          (LLit)
import qualified Luna.Syntax.Name.Pattern                 as Pattern
import           Luna.Util.LunaShow                       (LunaShow, lunaShow)



logger :: LoggerIO
logger = getLoggerIO $moduleName


type LunaExpr ae = (Enumerated ae, LunaShow (LExpr ae V), LunaShow (LLit ae))
type V = ()


run :: Monad m => StructInfo -> TDecl V -> EitherT State.Error m (TDecl V, Graph Tag V)
run aliasInfo lexpr = evalStateT (func2graph lexpr) $ State.mk aliasInfo Node.inputsID


func2graph :: TDecl V -> GBPass V m (TDecl V, Graph Tag V)
func2graph decl_ = do
    (inputsPos, outputPos, decl) <- State.initFreeNodeID decl_
    let (Label _ (Decl.Func (Decl.FuncDecl _ sig _ body))) = decl
    State.insNode (Node.inputsID, Node.Inputs inputsPos)
    State.insNode (Node.outputID, Node.Outputs def outputPos)
    sig'  <- buildInputsArgs sig
    body' <- buildBody body
    decl' <- State.saveFreeNodeID =<< State.connectMonadicOutput decl
    (,) <$> return (decl' & Label.element . Decl.funcDecl . Decl.funcDeclSig  .~ sig'
                          & Label.element . Decl.funcDecl . Decl.funcDeclBody .~ body')
        <*> State.getGraph


buildInputsArgs :: Pattern.ArgPat Tag e -> GBPass V m (Pattern.ArgPat Tag e)
buildInputsArgs (Pattern.NamePat prefix base segmentList) = flip evalStateT (0::Int) $ do
    prefix' <- case prefix of
        Nothing -> return Nothing
        Just pr -> Just <$> buildInputsArg pr
    base'        <- parseInputsSegment base
    segmentList' <- mapM parseInputsSegment segmentList
    return $ Pattern.NamePat prefix' base' segmentList'
    where
        parseInputsSegment (Pattern.Segment sBase sArgs) =
            Pattern.Segment sBase <$> mapM buildInputsArg sArgs

        buildInputsArg arg@(Arg pat _) = do
            no <- get
            put $ no + 1
            lift $ State.addToNodeMap (Enum.id $ pat ^. Label.label) (Node.inputsID, Port.mkSrc no)
            return arg


buildBody :: [TExpr V] -> GBPass V m [TExpr V]
buildBody []   = return []
buildBody body = do
    body' <- mapM (fmap (view _1) . flip buildNode Nothing) (init body)
    let output = last body
    output' <- buildOutput output
    return $ body' ++ [output']


buildOutput :: TExpr V -> GBPass V m (TExpr V)
buildOutput lexpr = case unwrap lexpr of
    Expr.Assignment {}                        -> view _1 <$> buildNode lexpr Nothing
    --Expr.Tuple   items                        -> buildAndConnectMany True  True Nothing Node.outputID items 0
    --Expr.Grouped (Label _ (Expr.Tuple items)) -> buildAndConnectMany True  True Nothing Node.outputID items 0
    --Expr.Grouped v@(Label _ (Expr.Var {}))    -> buildAndConnect     True  True Nothing Node.outputID (v, Port.Num 0)
    --Expr.Grouped V                            -> buildAndConnect     False True Nothing Node.outputID (v, Port.Num 0)
    --Expr.Var {}                               -> buildAndConnect     True  True Nothing Node.outputID (lexpr, Port.All)
    --_                                         -> buildAndConnect     False True Nothing Node.outputID (lexpr, Port.All)
    _   ->  do (lexpr', nodeID, srcPort) <- buildNode lexpr Nothing
               State.connect nodeID srcPort Node.outputID Port.mkDstAll
               return lexpr'


buildExprApp :: Expr.ExprApp Tag V -> GBPass V m (Expr.ExprApp Tag V, [ArgRef])
buildExprApp (Pattern.NamePat prefix base segmentList) = flip runStateT [] $ do
    prefix' <- case prefix of
        Nothing     -> return Nothing
        Just appArg -> Just <$> buildAppArg appArg
    base' <- buildBase base
    segmentList' <- mapM buildSegment segmentList
    modify reverse
    return $ Pattern.NamePat prefix' base' segmentList'
    where
        addArg arg = modify (arg:)

        buildBase (Pattern.Segment sBase sArgs) = do
            --port <- Port.mkDst . length <$> get
            --(sBase', arg) <- lift $ processArg (sBase, port)
            --addArg arg
            sArgs' <- mapM buildAppArg sArgs
            return $ Pattern.Segment sBase sArgs'

        buildSegment (Pattern.Segment sBase sArgs) =
            Pattern.Segment sBase <$> mapM buildAppArg sArgs

        buildAppArg (Expr.AppArg argName e) = do
            port <- Port.mkDst . length <$> get
            (e', arg) <- lift $ processArg (e, port)
            addArg arg
            return $ Expr.AppArg argName e'


buildNode :: TExpr V -> Maybe TPat -> GBPass V m (TExpr V, Node.ID, SrcPort)
buildNode lexpr outputName = case unwrap lexpr of
    Expr.Assignment dst src -> do
        (src', nodeID, srcPort) <- buildNode src (Just dst)
        State.registerIDs lexpr (nodeID, Port.mkSrcAll)
        return (Label tag $ Expr.Assignment dst src', nodeID, srcPort)
    Expr.Tuple items -> do
        (items', argRefs) <- processArgs items
        let lexpr' = Label tag $ Expr.Tuple items'
        (le, ni)  <- addNodeWithExpr lexpr' outputName (NodeExpr.StringExpr StringExpr.Tuple) argRefs
        State.registerIDs le (ni, Port.mkSrcAll)
        return (le, ni, Port.mkSrcAll)
    Expr.List (Expr.SeqList items) -> do
        (items', argRefs) <- processArgs items
        let lexpr' = Label tag $ Expr.List $ Expr.SeqList items'
        (le, ni)  <- addNodeWithExpr lexpr' outputName (NodeExpr.StringExpr StringExpr.List) argRefs
        State.registerIDs le (ni, Port.mkSrcAll)
        return (le, ni, Port.mkSrcAll)
    Expr.App exprApp -> do
        (exprApp', argRefs) <- buildExprApp exprApp
        let mp     = MultiPart.fromNamePat exprApp'
            lexpr' = Label tag $ Expr.App exprApp'
        (le, ni)  <- addNodeWithExpr lexpr' outputName (NodeExpr.MultiPart mp) argRefs
        State.registerIDs le (ni, Port.mkSrcAll)
        return (le, ni, Port.mkSrcAll)
    Expr.Var (Expr.Variable vname _) -> State.gvmNodeMapLookUp (Enum.id tag) >>= \case
        Nothing             -> do (le, ni) <- addNode lexpr outputName []
                                  return (le, ni, Port.mkSrcAll)
        Just (srcNID, srcPort) -> if Maybe.isJust outputName
            then do let nodeExpr = NodeExpr.StringExpr "_pattern_match_"
                    (le, ni) <- addNodeWithExpr lexpr outputName nodeExpr []
                    State.connect srcNID srcPort ni Port.mkDstAll
                    return (le, ni, Port.mkSrcAll)
            else do State.registerIDs lexpr (srcNID, srcPort)
                    return (lexpr, srcNID, srcPort)
    _ -> do
        (le, ni) <- addNode lexpr outputName []
        State.registerIDs le (ni, Port.mkSrcAll)
        return (le, ni, Port.mkSrcAll)
    where
        tag = lexpr ^. Label.label

processArgs :: [TExpr V] -> GBPass V m ([TExpr V], [ArgRef])
processArgs items = do
    itemsArgs <- mapM processArg $ zip items $ map Port.mkDst [0..]
    let items'  = map fst itemsArgs
        argRefs = map snd itemsArgs
    return (items', argRefs)


processArg :: (TExpr V, DstPort) -> GBPass V m (TExpr V, ArgRef)
processArg (lexpr, dstPort) = if constainsVar lexpr
    then do (lexpr', nodeID, srcPort) <- buildNode lexpr Nothing
            return (lexpr', ArgRef.mkNode (nodeID, srcPort, dstPort))
    else return (lexpr, ArgRef.mkDefault (dstPort, lexpr))


addNode :: TExpr V -> Maybe TPat -> [ArgRef] -> GBPass V m (TExpr V, Node.ID)
addNode lexpr outputName = addNodeWithExpr lexpr outputName nodeExpr
    where nodeExpr = NodeExpr.StringExpr $ fromString $ lunaShow lexpr


addNodeWithExpr :: TExpr V -> Maybe TPat
                -> NodeExpr Tag V -> [ArgRef] -> GBPass V m (TExpr V, Node.ID)
addNodeWithExpr lexpr outputName nodeExpr argRefs = do
    (nodeID, position, lexpr') <- State.getNodeInfo lexpr
    let defaults = ArgRef.defaults argRefs
        nodes    = ArgRef.nodes    argRefs
        node = Node.Expr nodeExpr outputName (DefaultsMap.fromList defaults) position
    State.insNode (nodeID, node)
    lexpr'' <- State.connectMonadic nodeID lexpr'
    mapM_ (\(srcID, srcPort, dstPort) -> State.connect srcID srcPort nodeID dstPort) nodes
    return (lexpr'', nodeID)


constainsVar _ = True
