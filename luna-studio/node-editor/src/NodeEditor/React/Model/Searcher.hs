module NodeEditor.React.Model.Searcher where

import           Common.Prelude
import qualified Data.Map.Lazy                              as Map
import qualified Data.Text                                  as Text
import qualified Data.Text.Span                             as Span
import           Luna.Syntax.Text.Lexer                     (Bound (Begin, End), Stream (Stream), Symbol (..), SymbolStream)
import qualified Luna.Syntax.Text.Lexer.Class               as Lexer
import           LunaStudio.Data.Node                       (ExpressionNode)
import qualified LunaStudio.Data.Node                       as Node
import           LunaStudio.Data.NodeLoc                    (NodeLoc)
import qualified LunaStudio.Data.NodeLoc                    as NodeLoc
import           LunaStudio.Data.PortRef                    (OutPortRef, srcNodeLoc)
import           LunaStudio.Data.Position                   (Position)
import qualified NodeEditor.Event.Shortcut                  as Shortcut
import qualified NodeEditor.React.Model.Node.ExpressionNode as Model
import           Text.ScopeSearcher.Item                    (Item (Element), Items)
import           Text.ScopeSearcher.QueryResult             (QueryResult)
import qualified Text.ScopeSearcher.QueryResult             as Result


data NewNode = NewNode { _position    :: Position
                       , _predPortRef :: Maybe OutPortRef
                       } deriving (Eq, Generic, Show)

data NodeModeInfo = NodeModeInfo { _className   :: Maybe Text
                                 , _newNodeData :: Maybe NewNode
                                 , _argNames    :: [Text]
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

data Mode = Command                       [QueryResult ()]
          | Node     NodeLoc NodeModeInfo [QueryResult ExpressionNode]
          | NodeName NodeLoc              [QueryResult ExpressionNode]
          | PortName OutPortRef           [QueryResult ExpressionNode]
          deriving (Eq, Generic, Show)

data Searcher = Searcher
      { _selected      :: Int
      , _mode          :: Mode
      , _input         :: Input
      , _replaceInput  :: Bool
      , _rollbackReady :: Bool
      } deriving (Eq, Generic, Show)

makeLenses ''Searcher
makeLenses ''DividedInput
makePrisms ''Input
makePrisms ''Mode

instance Default Input where def = Raw def

inputText :: Getter Searcher Text
inputText = to (toText . view input)

toText :: Input -> Text
toText (Raw t)                        = t
toText (Divided (DividedInput p q s)) = p <> q <> s

--TODO[LJK]: Once WD fixes convert use convert on Stream instead of passing input
fromStream :: Text -> SymbolStream Text -> Int -> Input
fromStream input' inputStream pos = getInput (findQueryBegin inputStream pos) pos where
    isQuery :: Symbol Text -> Bool
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
    inString :: Symbol Text -> Bool
    inString (Str    {})     = True
    inString (StrEsc {})     = True
    inString (Quote _ Begin) = True
    inString _               = False
    findQueryBegin :: SymbolStream Text -> Int -> Maybe Int
    findQueryBegin (Stream [])    _ = Nothing
    findQueryBegin (Stream (h:t)) p = do
        let tokenLength = fromIntegral . unwrap $ h ^. Lexer.span . Span.length + h ^. Lexer.span . Span.offset
        if p > tokenLength
            then (tokenLength +) <$> findQueryBegin (Stream t) (p - tokenLength)
        else if p <= (fromIntegral . unwrap $ h ^. Lexer.span . Span.length)
            then if isQuery (h ^. Lexer.token_elem) then Just 0 else Nothing
        else if inString (h ^. Lexer.token_elem)
            then Nothing
            else Just p
    getInput :: Maybe Int -> Int -> Input
    getInput Nothing    _   = Raw input'
    getInput (Just beg) end = do
        let (pref', suff) = Text.splitAt end input'
            (pref , q)    = Text.splitAt beg pref'
        Divided $ DividedInput pref q suff

findLambdaArgsAndEndOfLambdaArgs :: SymbolStream Text -> Maybe ([Text], Int)
findLambdaArgsAndEndOfLambdaArgs (Stream tokens) = findRecursive tokens (0 :: Int) 0 def def where
    exprLength   t = fromIntegral . unwrap $ t ^. Lexer.span . Span.length
    offsetLength t = fromIntegral . unwrap $ t ^. Lexer.span . Span.offset
    tokenLength  t = exprLength t + offsetLength t
    findRecursive []    _                     _      _    res = res
    findRecursive (h:t) openParanthesisNumber endPos args res = case h ^. Lexer.token_elem of
        BlockStart    -> if openParanthesisNumber == 0
                    then findRecursive t openParanthesisNumber        (endPos + tokenLength h) args $ Just (args, endPos + exprLength h)
                    else findRecursive t openParanthesisNumber        (endPos + tokenLength h) args $ res
        Var        {} -> findRecursive t openParanthesisNumber        (endPos + tokenLength h) (h ^. Lexer.src : args) res
        Block Begin   -> findRecursive t (succ openParanthesisNumber) (endPos + tokenLength h) args res
        Block End     -> findRecursive t (pred openParanthesisNumber) (endPos + tokenLength h) args res
        _             -> findRecursive t openParanthesisNumber        (endPos + tokenLength h) args res

mkDef :: Mode -> Searcher
mkDef mode' = Searcher def mode' (Divided $ DividedInput def def def) False False

defCommand :: Searcher
defCommand = mkDef $ Command def

selectedExpression :: Getter Searcher (Maybe Text)
selectedExpression = to getExpression where
    getExpression s = let i = s ^. selected in
        if i == 0 then Nothing else case s ^. mode of
            Command    results -> Result._name <$> results ^? ix (i - 1)
            Node   _ _ results -> view Node.expression . Result._element <$> results ^? ix (i - 1)
            NodeName _ results -> view Node.expression . Result._element <$> results ^? ix (i - 1)
            PortName _ results -> view Node.expression . Result._element <$> results ^? ix (i - 1)

selectedNode :: Getter Searcher (Maybe ExpressionNode)
selectedNode = to getNode where
    getNode s = let i = s ^. selected in
        if i == 0 then Nothing else case s ^. mode of
            Node _ _ results -> Result._element <$> results ^? ix (i - 1)
            _                -> Nothing

applyExpressionHint :: ExpressionNode -> Model.ExpressionNode -> Model.ExpressionNode
applyExpressionHint n exprN = exprN & Model.expression .~ n ^. Node.expression
                                    & Model.canEnter   .~ n ^. Node.canEnter
                                    & Model.inPorts    .~ (convert <$> n ^. Node.inPorts)
                                    & Model.outPorts   .~ (convert <$> n ^. Node.outPorts)
                                    & Model.code       .~ n ^. Node.code

resultsLength :: Getter Searcher Int
resultsLength = to getLength where
    getLength searcher = case searcher ^. mode of
      Command    results -> length results
      Node   _ _ results -> length results
      NodeName _ results -> length results
      PortName _ results -> length results

updateNodeResult :: [QueryResult ExpressionNode] -> Mode -> Mode
updateNodeResult r (Node nl nmi _) = Node nl nmi r
updateNodeResult _ m               = m

updateNodeArgs :: [Text] -> Mode -> Mode
updateNodeArgs args (Node nl (NodeModeInfo cn nnd _) r) = (Node nl (NodeModeInfo cn nnd args) r)
updateNodeArgs _ m                                      = m

updateCommandsResult :: [QueryResult ()] -> Mode -> Mode
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

isSearcherRelated :: NodeLoc -> Searcher -> Bool
isSearcherRelated nl s = isPrefixOf nlIdPath sIdPath where
    nlIdPath = NodeLoc.toNodeIdList nl
    sIdPath = case s ^. mode of
        Node     snl   _ _ -> NodeLoc.toNodeIdList snl
        NodeName snl     _ -> NodeLoc.toNodeIdList snl
        PortName portRef _ -> NodeLoc.toNodeIdList $ portRef ^. srcNodeLoc
        _                  -> []

data OtherCommands = AddNode
                   deriving (Bounded, Enum, Eq, Generic, Read, Show)

allCommands :: Items ()
allCommands = Map.fromList $ (, Element ()) . convert <$> (commands <> otherCommands) where
    commands = show <$> [(minBound :: Shortcut.Command) ..]
    otherCommands = show <$> [(minBound :: OtherCommands)]
