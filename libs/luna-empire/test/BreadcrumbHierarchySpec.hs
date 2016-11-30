module BreadcrumbHierarchySpec (spec) where

import           Prelude

import           Data.List                  (delete, find, partition)
import           Data.Tree
import           Data.UUID                  (UUID, fromWords)

import           Empire.Data.BreadcrumbHierarchy
import           Empire.API.Data.Breadcrumb (Breadcrumb(..), BreadcrumbItem(..))
import           Empire.API.Data.Node       (NodeId)

import           Test.Hspec                 (Spec, describe, it)



uuid :: Int -> UUID
uuid i = fromWords 0 0 0 (fromIntegral i)

forest123 = addID (uuid 3) $ addID (uuid 2) $ addID (uuid 1) empty
forest4 = addID (uuid 4) empty

addToTree :: Breadcrumb BreadcrumbItem -> Int -> BreadcrumbHierarchy -> Maybe BreadcrumbHierarchy
addToTree b i h | Just h' <- h `navigateTo` b = replaceAt b (addID (uuid i) h') h
                | otherwise = Nothing

spec :: Spec
spec = describe "forest" $ do
    let breadcrumb1 = Breadcrumb [Lambda $ uuid 1]
    let Just nested4 = replaceAt breadcrumb1 forest4 forest123
    let breadcrumb14 = Breadcrumb [Lambda (uuid 1), Lambda (uuid 4)]
    let Just nested4nested5 = replaceAt breadcrumb14 (addID (uuid 5) empty) nested4
    it "extracts subtree" $ do
        nested4 `navigateTo` breadcrumb1 == Just forest4
    it "adds node two levels deep" $ do
        let Just res2 = addToTree breadcrumb14 5 nested4
        res2 == nested4nested5
    it "adds at the top level" $ do
        let Just x = addToTree (Breadcrumb []) 4 forest123
        topLevelIDs x == [uuid 4, uuid 3, uuid 2, uuid 1]
    it "fails on non-existent breadcrumb" $
        forest123 `navigateTo` (Breadcrumb [Lambda (uuid 9)]) == Nothing
    it "navigates two level deep" $
        nested4nested5 `navigateTo` breadcrumb14 == Just (addID (uuid 5) empty)
    it "replaceAt fails on non-existent breadcrumb" $
        replaceAt (Breadcrumb [Lambda (uuid 9)]) empty forest123 == Nothing
    it "removes from top level" $
        topLevelIDs (removeID (uuid 1) forest123) == [uuid 3, uuid 2]
    it "adds with nested node" $ do
        let hierarchy = addID (uuid 1) empty
            given = addWithLeafs (uuid 2) [uuid 3, uuid 4] hierarchy
            when  = topLevelIDs <$> given `navigateTo` (Breadcrumb [Lambda $ uuid 2])
        when == Just [uuid 3, uuid 4]
