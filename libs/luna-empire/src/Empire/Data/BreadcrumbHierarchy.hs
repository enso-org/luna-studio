module Empire.Data.BreadcrumbHierarchy (
      BreadcrumbHierarchy
    , empty
    , replaceAt
    , navigateTo
    , addID
    , addWithLeafs
    , removeID
    , topLevelIDs
    ) where

import           Prologue hiding (at)

import           Data.List (delete, filter, find)
import           Data.Tree

import           Empire.API.Data.Breadcrumb (Breadcrumb (..), BreadcrumbItem (..))
import           Empire.API.Data.Node       (NodeId)


newtype BreadcrumbHierarchy = BC (Forest NodeId) deriving (Eq, Show)

empty :: BreadcrumbHierarchy
empty = BC []

replaceAt :: Breadcrumb BreadcrumbItem -> BreadcrumbHierarchy -> BreadcrumbHierarchy -> Maybe BreadcrumbHierarchy
replaceAt (Breadcrumb bs) (BC forest) (BC hierarchy) = BC <$> go bs forest hierarchy
    where
      go :: [BreadcrumbItem] -> Forest NodeId -> Forest NodeId -> Maybe (Forest NodeId)
      go [] newForest _ = Just newForest
      go (Lambda b:bs) newForest forest = case find (\(Node l _) -> b == l) forest of
          Just a@(Node b f) -> case go bs newForest f of
              Just x -> Just $ Node b x : delete a forest
              _      -> Nothing
          _                 -> Nothing

navigateTo :: BreadcrumbHierarchy -> Breadcrumb BreadcrumbItem -> Maybe BreadcrumbHierarchy
navigateTo (BC forest) (Breadcrumb breadcrumbs) = BC <$> go forest nodeIds
    where
      nodeIds = map (\(Lambda id) -> id) breadcrumbs
      go :: Forest NodeId -> [NodeId] -> Maybe (Forest NodeId)
      go forest [] = Just forest
      go forest (b:bs) | Just f <- at b forest = go f bs
                       | otherwise = Nothing

at :: NodeId -> Forest NodeId -> Maybe (Forest NodeId)
at nid forest = snd <$> find ((==) nid . fst) topLevel
    where
      topLevel = map (\a -> (rootLabel a, subForest a)) forest

addID :: NodeId -> BreadcrumbHierarchy -> BreadcrumbHierarchy
addID nodeid (BC hierarchy) = BC $ Node nodeid [] : hierarchy

addWithLeafs :: NodeId -> [NodeId] -> BreadcrumbHierarchy -> BreadcrumbHierarchy
addWithLeafs nodeid leafs (BC hierarchy) = BC $ Node nodeid children : hierarchy
    where
        children = map (\nid -> Node nid []) leafs

removeID :: NodeId -> BreadcrumbHierarchy -> BreadcrumbHierarchy
removeID nodeid (BC hierarchy) = BC $ filter (\(Node nid _) -> nid /= nodeid) hierarchy

topLevelIDs :: BreadcrumbHierarchy -> [NodeId]
topLevelIDs (BC f) = map (\(Node nid _) -> nid) f
