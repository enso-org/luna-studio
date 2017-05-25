module LunaStudio.Data.NodeSearcher
    ( module X
    , module LunaStudio.Data.NodeSearcher
    ) where

import qualified Data.Map.Lazy           as Map
import           Data.Text               (Text)
import qualified Data.UUID.Types         as UUID
import           LunaStudio.Data.Node    (ExpressionNode, mkExprNode)
import           Prologue                hiding (Item)
import           Text.ScopeSearcher.Item as X

mockNode :: Text -> ExpressionNode
mockNode expr = mkExprNode (fromJust $ UUID.fromString "094f9784-3f07-40a1-84df-f9cf08679a27") expr def

entry :: Text -> (Text, Item ExpressionNode)
entry name = (name, Element $ mockNode name)

methodEntry :: Text -> [Text] -> (Text, Item ExpressionNode)
methodEntry className methodList = (className, Group (Map.fromList $ entry <$> methodList) $ mockNode className)
