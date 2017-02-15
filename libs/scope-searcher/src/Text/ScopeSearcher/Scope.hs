{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.ScopeSearcher.Scope (
      moduleItems
    , searchInScope
    , appendPath
    ) where

import           Prelude

import           Control.Lens
import           Data.Default
import qualified Data.Map                       as Map
import           Data.Monoid                    ((<>))
import           GHC.Exts                       (sortWith)

import           Data.Text                      (Text)
import qualified Data.Text                      as Text

import           Text.ScopeSearcher.Searcher    (Match (..), Nameable, Submatch (..), Weightable)
import qualified Text.ScopeSearcher.Searcher    as Searcher

import           Text.ScopeSearcher.Item        (Item (..), Items)
import qualified Text.ScopeSearcher.Item        as Item
import           Text.ScopeSearcher.QueryResult (Highlight (..), QueryResult (..))

type Path = [Text]


data SearchableItem a = SearchableItem
        { _itemWeight :: Double
        , _itemPath   :: Text
        , _itemName   :: Text
        , _itemTpe    :: Item a
        } deriving (Eq, Show)

instance Nameable (SearchableItem a) where
    -- name (SearchableItem _ p n _) = appendPath p n    -- search in full path
    name (SearchableItem _ p n _) = n

instance Weightable (SearchableItem a) where
    weight (SearchableItem w _ _ _) = w

makeLenses ''SearchableItem

jsItemType :: Item a -> Text
jsItemType (Element _) = "function"
jsItemType (Group _ _) = "module"


searchableItems' :: Double -> Text -> Items a -> [SearchableItem a]
searchableItems' weight prefix items = Map.foldMapWithKey addItems items where
    addItems :: Text -> Item a -> [SearchableItem a]
    addItems name i@(Element _) = [SearchableItem weight prefix name i]
    addItems name i@(Group m _) = [SearchableItem weight prefix name i] ++ (searchableItems' (weight * 0.99) (nameWithPrefix prefix name) m)
    nameWithPrefix "" name = name
    nameWithPrefix prefix name = prefix <> "." <> name

searchableItems :: Items a -> [SearchableItem a]
searchableItems = searchableItems' 1.0 ""

moduleByPath :: Items a -> Path -> Maybe (Items a)
moduleByPath m []     = Just m
moduleByPath m [""]   = Just m
moduleByPath m [x]    = case (Map.lookup x m) of
    Just (Group m _) -> Just m
    _                -> Nothing
moduleByPath m (x:xs) = case (Map.lookup x m) of
    Just (Group m _) -> moduleByPath m xs
    _                -> Nothing

pathFromText :: Text -> [Text]
pathFromText = Text.splitOn "."

appendPath :: Text -> Text -> Text
appendPath "" n = n
appendPath p  n = p <> "." <> n

itemsInScope :: Items a -> Text -> [SearchableItem a]
itemsInScope root path = case moduleByPath root (pathFromText path) of
    Just items -> searchableItems items
    Nothing    -> []

searchInScope :: Eq a => Items a -> Text -> [QueryResult a]
searchInScope root expr
    | (Text.length expr > 0) && (Text.last expr == '.') = maybe [] displayModuleItems scope
    | otherwise                                         = fmap transformMatch $ Searcher.findSuggestions items query
    where
        (prefixWithDot, query) = Text.breakOnEnd "." expr
        prefix = case prefixWithDot of
            "" -> ""
            t  -> Text.init t
        scope = moduleByPath root $ pathFromText prefix
        items = maybe [] searchableItems scope
        transformMatch (Match score (SearchableItem _ path name tpe) sm) = QueryResult path name (appendPath path name) (fmap toHighlight sm) (jsItemType tpe) score (tpe ^. Item.element)
        displayModuleItems items = fmap di $ Map.toList items
        di  (name, t)  = QueryResult "" name name [] (jsItemType t) def $ t ^. Item.element

moduleItems :: forall a. Items a -> Text -> [QueryResult a]
moduleItems root path = fmap toQueryResult $ items where
    toQueryResult :: (Text, Item a) -> QueryResult a
    toQueryResult (name, t)  = QueryResult path name (appendPath path name) [] (jsItemType t) def (t ^. Item.element)
    items :: [(Text, Item a)]
    items = case moduleByPath root (pathFromText path) of
        Just items -> sortWith fst $ Map.toList items
        Nothing              -> []

toHighlight :: Submatch -> Highlight
toHighlight (Submatch s l) = Highlight (fromIntegral s) (fromIntegral l)
