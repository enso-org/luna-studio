{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module NodeEditor.View.Searcher where

import           Common.Data.JSON                (toJSONVal)
import           Common.Prelude
import qualified Control.Lens.Aeson              as Lens
import           Data.Aeson                      (ToJSON (toEncoding, toJSON))
import           Data.Convert                    (Convertible (convert))
import           NodeEditor.React.Model.Searcher (Searcher)
import qualified NodeEditor.React.Model.Searcher as Searcher
import           NodeEditor.View.Diff            (DiffT, diffApply)
import           LunaStudio.Data.NodeSearcher    (Match)
import qualified LunaStudio.Data.NodeSearcher    as Match



data HighlightView = HighlightView
    { _start :: Int
    , _end   :: Int
    } deriving (Generic, Show)

data EntryView = EntryView
    { _name       :: String
    , _doc        :: String
    , _className  :: String
    , _highlights :: [HighlightView]
    } deriving (Generic, Show)

data SearcherView = SearcherView
    { _key      :: Maybe String
    , _selected :: Int
    , _entries  :: [EntryView]
    , _input    :: String
    } deriving (Generic, Show)

makeLenses ''HighlightView
makeLenses ''EntryView
makeLenses ''SearcherView

instance ToJSON HighlightView where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON

instance ToJSON EntryView     where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON

instance ToJSON SearcherView  where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON

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
        {- entries  -} (s ^. Searcher.hints . to convert)
        {- input    -} "test"

nodeKey' :: Searcher.Mode -> Maybe String
nodeKey' = \case
    Searcher.Node nl _ _   -> Just $ show nl
    Searcher.NodeName nl _ -> Just $ show nl
    _                      -> Nothing

foreign import javascript safe "atomCallback.getNodeEditorView().setSearcher($1)"
    setSearcher__ :: JSVal -> IO ()

setSearcher :: MonadIO m => Maybe SearcherView -> m ()
setSearcher = liftIO . setSearcher__ <=< toJSONVal

searcherView :: MonadIO m => DiffT (Maybe Searcher) m ()
searcherView = diffApply $ setSearcher . convert
