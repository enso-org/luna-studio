{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module NodeEditor.React.Model.Searcher
    ( module NodeEditor.React.Model.Searcher
    , Match
    ) where

import           Common.Prelude
import qualified Data.Map.Lazy                              as Map
import qualified Data.Text                                  as Text
import           Data.Text32                                (Text32)
import qualified Data.UUID.Types                            as UUID
import qualified Data.Vector.Unboxed                        as Vector
import           Luna.Syntax.Text.Lexer                     (Bound (Begin, End), Symbol (..), Token)
import qualified Luna.Syntax.Text.Lexer                     as Lexer
import           LunaStudio.Data.Node                       (ExpressionNode, mkExprNode)
import qualified LunaStudio.Data.Node                       as Node
import           LunaStudio.Data.NodeLoc                    (NodeLoc)
import qualified LunaStudio.Data.NodeLoc                    as NodeLoc
import           LunaStudio.Data.NodeSearcher               (Match)
import qualified LunaStudio.Data.NodeSearcher               as NS
import           LunaStudio.Data.PortRef                    (OutPortRef (OutPortRef), srcNodeLoc)
import qualified LunaStudio.Data.PortRef                    as PortRef
import           LunaStudio.Data.Position                   (Position)
import           LunaStudio.Data.TypeRep                    (TypeRep (TCons))
import qualified NodeEditor.Event.Shortcut                  as Shortcut
import qualified NodeEditor.React.Model.Node.ExpressionNode as Model
import qualified NodeEditor.React.Model.Port                as Port
import           NodeEditor.React.Model.Visualization       (RunningVisualization)
import           Prologue                                   (unsafeFromJust)


data NewNode = NewNode { _position    :: Position
                       , _predPortRef :: Maybe OutPortRef
                       } deriving (Eq, Generic, Show)

data NodeModeInfo = NodeModeInfo { _className   :: Maybe Text
                                 , _newNodeData :: Maybe NewNode
                                 , _argNames    :: [Text]
                                 , _docVisInfo  :: Maybe RunningVisualization
                                 } deriving (Eq, Generic, Show)
makeLenses ''NewNode
makeLenses ''NodeModeInfo

data DividedInput = DividedInput { _prefix :: Text
                                 , _query  :: Text
                                 , _suffix :: Text
                                 } deriving (Eq, Generic, Show)


data Input = Raw     Text
           | Divided DividedInput
           deriving (Eq, Generic, Show)

data Mode = Command                       [Match]
          | Node     NodeLoc NodeModeInfo [Match]
          | NodeName NodeLoc              [Match]
          | PortName OutPortRef           [Match]
          deriving (Eq, Generic, Show)

data Searcher = Searcher
              { _selected      :: Int
              , _mode          :: Mode
              , _input         :: Input
              , _replaceInput  :: Bool
              , _rollbackReady :: Bool
              , _waitingForTc  :: Bool
              , _searcherError :: Maybe Text
              } deriving (Eq, Generic, Show)

makeLenses ''Searcher
makeLenses ''DividedInput
makePrisms ''Input
makePrisms ''Mode

instance Default Input where def = Raw def

predNl :: Getter Searcher (Maybe NodeLoc)
predNl = to predNl' where
    predNl' s = s ^? mode . _Node . _2 . newNodeData . _Just . predPortRef . _Just . PortRef.nodeLoc

docVis :: Getter Searcher (Maybe RunningVisualization)
docVis = to docVis' where
    docVis' s = case s ^. mode of
      Node _ nmi _ -> nmi ^. docVisInfo
      _            -> Nothing

inputText :: Getter Searcher Text
inputText = to (toText . view input)

toText :: Input -> Text
toText (Raw t)                        = t
toText (Divided (DividedInput p q s)) = p <> q <> s

fromStream :: Text -> [Token Symbol] -> Int -> Input
fromStream input' inputStream pos = getInput (findQueryBegin inputStream pos) pos where
    isQuery :: Symbol -> Bool
    isQuery (Var      {}) = True
    isQuery (Cons     {}) = True
    isQuery (Wildcard {}) = True
    isQuery (KwAll    {}) = True
    isQuery (KwCase   {}) = True
    isQuery (KwClass  {}) = True
    isQuery (KwDef    {}) = True
    isQuery (KwImport {}) = True
    isQuery (KwOf     {}) = True
    isQuery (Operator {}) = True
    isQuery (Modifier {}) = True
    isQuery _             = False
    inString :: Symbol -> Bool
    inString (Str    {})     = True
    inString (StrEsc {})     = True
    inString (Quote _ Begin) = True
    inString _               = False
    findQueryBegin :: [Token Symbol] -> Int -> Maybe Int
    findQueryBegin []    _ = Nothing
    findQueryBegin (h:t) p = do
        let tokenLength = fromIntegral $ h ^. Lexer.span + h ^. Lexer.offset
        if p > tokenLength
            then (tokenLength +) <$> findQueryBegin t (p - tokenLength)
        else if p <= (fromIntegral $ h ^. Lexer.span)
            then if isQuery (h ^. Lexer.element) then Just 0 else Nothing
        else if inString (h ^. Lexer.element)
            then Nothing
            else Just p
    getInput :: Maybe Int -> Int -> Input
    getInput Nothing    _   = Raw input'
    getInput (Just beg) caretPos = do
        let (pref', suff') = Text.splitAt caretPos input'
            (pref , q')    = Text.splitAt beg pref'
            (q'', suff)    = Text.breakOn " " suff'
        Divided $ DividedInput pref (q' <> q'') suff

findLambdaArgsAndEndOfLambdaArgs :: Text32 -> [Token Symbol] -> Maybe ([String], Int)
findLambdaArgsAndEndOfLambdaArgs input' tokens = findRecursive tokens (0 :: Int) 0 def def where
    exprLength   t = fromIntegral $ t ^. Lexer.span
    offsetLength t = fromIntegral $ t ^. Lexer.offset
    getArg   beg t = Vector.slice beg (exprLength t) input'
    tokenLength  t = exprLength t + offsetLength t
    findRecursive []    _                     _      _    res = res
    findRecursive (h:t) openParanthesisNumber endPos args res = case h ^. Lexer.element of
        BlockStart    -> if openParanthesisNumber == 0
                    then findRecursive t openParanthesisNumber        (endPos + tokenLength h) args $ Just (convert <$> args, endPos + exprLength h)
                    else findRecursive t openParanthesisNumber        (endPos + tokenLength h) args $ res
        Var        {} -> findRecursive t openParanthesisNumber        (endPos + tokenLength h) (getArg endPos h : args) res
        Block Begin   -> findRecursive t (succ openParanthesisNumber) (endPos + tokenLength h) args res
        Block End     -> findRecursive t (pred openParanthesisNumber) (endPos + tokenLength h) args res
        _             -> findRecursive t openParanthesisNumber        (endPos + tokenLength h) args res

mkDef :: Mode -> Searcher
mkDef mode' = Searcher def mode' (Divided $ DividedInput def def def) False False False def

defCommand :: Searcher
defCommand = mkDef $ Command def

hints :: Lens' Searcher [Match]
hints = lens getHints setHints where
  getHints s = case s ^. mode of
    Command    results -> results
    Node   _ _ results -> results
    NodeName _ results -> results
    PortName _ results -> results
  setHints s h = case s ^. mode of
    Command     results -> s & mode .~ Command h
    Node nl nmi results -> s & mode .~ Node nl nmi h
    NodeName nl results -> s & mode .~ NodeName nl h
    PortName pr results -> s & mode .~ PortName pr h

isValidSelection :: Int -> Searcher -> Bool
isValidSelection i s = i < 0 || i > length (s ^. hints)

selectedMatch :: Getter Searcher (Maybe Match)
selectedMatch = to selectedMatch where
    selectedMatch s = let i = s ^. selected - 1 in
        if i < 0 then Nothing else s ^? hints . ix i

selectedNode :: Getter Searcher (Maybe ExpressionNode)
selectedNode = to getNode where
    mockNode expr = mkExprNode (unsafeFromJust $ UUID.fromString "094f9784-3f07-40a1-84df-f9cf08679a27") expr def
    getNode s = let i = s ^. selected in
        if i == 0 then Nothing else case s ^. mode of
            Node _ _ hints' -> mockNode . view NS.name <$> hints' ^? ix (i - 1)
            _               -> Nothing

applyExpressionHint :: ExpressionNode -> Model.ExpressionNode -> Model.ExpressionNode
applyExpressionHint n exprN = exprN & Model.expression .~ n ^. Node.expression
                                    & Model.canEnter   .~ n ^. Node.canEnter
                                    & Model.inPorts    .~ (convert <$> n ^. Node.inPorts)
                                    & Model.outPorts   .~ (convert <$> n ^. Node.outPorts)
                                    & Model.code       .~ n ^. Node.code

updateNodeResult :: [Match] -> Mode -> Mode
updateNodeResult r (Node nl nmi _) = Node nl nmi r
updateNodeResult _ m               = m

updateNodeArgs :: [Text] -> Mode -> Mode
updateNodeArgs args (Node nl (NodeModeInfo cn nnd _ vis) r) = (Node nl (NodeModeInfo cn nnd args vis) r)
updateNodeArgs _ m                                      = m

updateCommandsResult :: [Match] -> Mode -> Mode
updateCommandsResult r (Command _) = Command r
updateCommandsResult _ m           = m

isCommand :: Getter Searcher Bool
isCommand = to matchCommand where
  matchCommand searcher = case searcher ^. mode of
        Command {} -> True
        _          -> False

isNode :: Getter Searcher Bool
isNode = to matchNode where
    matchNode searcher = case searcher ^. mode of
        Node {} -> True
        _       -> False

isNodeName :: Getter Searcher Bool
isNodeName = to matchNodeName where
    matchNodeName searcher = case searcher ^. mode of
        NodeName {} -> True
        _           -> False

data OtherCommands = AddNode
                   deriving (Bounded, Enum, Eq, Generic, Read, Show)

allCommands :: [Text]
allCommands = convert <$> (commands <> otherCommands) where
    commands = show <$> [(minBound :: Shortcut.Command) ..]
    otherCommands = show <$> [(minBound :: OtherCommands)]


getPredInfo :: Model.ExpressionNode -> (Maybe Text, Maybe OutPortRef)
getPredInfo pred = (className, predPortRef) where
    mayP        = listToMaybe $ Model.outPortsList pred
    className   = case view Port.valueType <$> mayP of
        Just (TCons cn _) -> Just $ convert cn
        _                 -> Nothing
    predPortRef = OutPortRef (pred ^. Model.nodeLoc) . view Port.portId <$> mayP
