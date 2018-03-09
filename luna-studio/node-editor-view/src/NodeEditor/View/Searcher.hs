{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module NodeEditor.View.Searcher where

import           Common.Data.JSON                (toJSONVal)
import           Common.Prelude
import           Data.Aeson                      (ToJSON)
import           Data.Convert                    (Convertible (convert))
import           NodeEditor.React.Model.Searcher (Searcher)
import qualified NodeEditor.React.Model.Searcher as Searcher
import           NodeEditor.View.Diff            (DiffT, diffApply)
import           LunaStudio.Data.NodeSearcher    (Match)
import qualified LunaStudio.Data.NodeSearcher    as Match



searcherView :: MonadIO m => DiffT (Maybe Searcher) m ()
searcherView = diffApply $ setSearcher . convert

data HighlightView = HighlightView
    { start :: Int
    , end   :: Int
    } deriving (Generic, Show)

data EntryView = EntryView
    { name       :: String
    , doc        :: String
    , className  :: String
    , highlights :: [HighlightView]
    } deriving (Generic, Show)

data SearcherView = SearcherView
    { key      :: Maybe String
    , selected :: Int
    , entries  :: [EntryView]
    , input    :: String
    } deriving (Generic, Show)

instance ToJSON HighlightView
instance ToJSON EntryView
instance ToJSON SearcherView

instance Convertible (Int, Int) HighlightView where
    convert = uncurry HighlightView

instance Convertible Match EntryView where
    convert m = EntryView
        {- name       -} (m ^. Match.name . to convert)
        {- doc        -} (m ^. Match.doc . to convert)
        {- className  -} (m ^. Match.className . to convert)
        {- highlights -} (m ^. Match.match . to convert)

instance Convertible Searcher SearcherView where
    convert s = SearcherView
        {- key      -} (s ^. Searcher.mode . to nodeKey')
        {- selected -} (s ^. Searcher.selected)
        {- entries  -} (s ^. Searcher.mode . to entries')
        {- input    -} "test"

nodeKey' :: Searcher.Mode -> Maybe String
nodeKey' = \case
    Searcher.Node nl _ _   -> Just $ show nl
    Searcher.NodeName nl _ -> Just $ show nl
    _                      -> Nothing

entries' :: Searcher.Mode -> [EntryView]
entries' = \case
    Searcher.Command    m -> convert m
    Searcher.Node   _ _ m -> convert m
    Searcher.NodeName _ m -> convert m
    Searcher.PortName _ m -> convert m

foreign import javascript safe "atomCallback.getNodeEditorView().setSearcher($1)"
    setSearcher__ :: JSVal -> IO ()

setSearcher :: MonadIO m => Maybe SearcherView -> m ()
setSearcher = liftIO . setSearcher__ <=< toJSONVal
