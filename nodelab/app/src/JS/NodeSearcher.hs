module JS.NodeSearcher
    ( initNodeSearcher
    , displayQueryResults
    , displayTreeResults
    , TargetSearcher (..)
    ) where

import           Data.JSString.Text             (textToJSString)
import           Luna.Studio.Data.Vector        (Position, x, y)
import           Luna.Studio.Prelude

import           Data.Aeson                     (toJSON)
import           Empire.API.Data.Node           (NodeId)
import           Empire.API.JSONInstances       ()

import           Text.ScopeSearcher.QueryResult (Highlight (..), QueryResult (..))


foreign import javascript safe "require('node_searcher').create($1, $2, $3, $4, $5)"
    initNodeSearcher' :: JSString -> JSVal -> Double -> Double -> Bool -> IO ()

initNodeSearcher :: Text -> Maybe NodeId -> Position -> Bool -> IO ()
initNodeSearcher expr nodeId pos command = do
    nodeId' <- toJSVal $ toJSON nodeId
    initNodeSearcher' (textToJSString expr) nodeId' (pos ^. x) (pos ^. y) command

-- display results

data TargetSearcher = NodeSearcher | CommandSearcher

newtype JSHighlight = JSHighlight JSVal

foreign import javascript safe "if(require('common').nodeSearcher) require('common').nodeSearcher.clearResults()"
    nodesearcher_clear_results :: IO ()

foreign import javascript safe "if(require('common').nodeSearcher) require('common').nodeSearcher.addResult($1, $2, $3, $4, $5)"
    nodesearcher_add_result :: JSString -> JSString -> JSString -> JSHighlight -> JSString -> IO ()

foreign import javascript safe "if(require('common').nodeSearcher) require('common').nodeSearcher.finishResult()"
    nodesearcher_finish_result :: IO ()

foreign import javascript safe "require('common').nodeSearcher.addTreeResult($1, $2, $3, $4)"
    nodesearcher_add_tree_result :: JSString -> JSString -> JSString -> JSString -> IO ()

searcherResultName :: TargetSearcher -> QueryResult -> Text
searcherResultName NodeSearcher    (QueryResult _ name     _ _ _ _) = name
searcherResultName CommandSearcher (QueryResult _ _ fullname _ _ _) = fullname

displayQueryResult :: TargetSearcher -> QueryResult -> IO ()
displayQueryResult target qr@(QueryResult prefix name _fullname highlight tpe _) = do
    ary <- createJSArray
    mapM_ (pushHighlight ary) highlight
    let targetName = searcherResultName target qr
    nodesearcher_add_result (textToJSString prefix) (textToJSString name) (textToJSString targetName) ary (textToJSString tpe)
    nodesearcher_finish_result

displayQueryResults :: TargetSearcher -> [QueryResult] -> IO ()
displayQueryResults target results = do
    nodesearcher_clear_results
    forM_ results $ displayQueryResult target

displayTreeResult :: TargetSearcher -> QueryResult -> IO ()
displayTreeResult target qr@(QueryResult prefix name _fullname _ tpe _) = do
    let targetName = searcherResultName target qr
    nodesearcher_add_tree_result (textToJSString prefix) (textToJSString name) (textToJSString targetName) (textToJSString tpe)

displayTreeResults :: TargetSearcher -> [QueryResult] -> IO ()
displayTreeResults target results = forM_ results $ displayTreeResult target

foreign import javascript safe "[]"
    createJSArray :: IO JSHighlight

foreign import javascript safe "$1.push({start: $2, length: $3})"
    pushHighlightJs :: JSHighlight -> Int -> Int -> IO JSHighlight

pushHighlight :: JSHighlight -> Highlight -> IO JSHighlight
pushHighlight acc (Highlight start len) = pushHighlightJs acc start len
