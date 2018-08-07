{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module NodeEditor.View.Searcher where

import           Common.Data.JSON                (toJSONVal)
import           Common.Prelude
import qualified Control.Lens.Aeson              as Lens
import           Data.Aeson                      (ToJSON (toEncoding, toJSON))
import           Data.Convert                    (Convertible (convert))
import           LunaStudio.Data.NodeSearcher    (Match)
import qualified LunaStudio.Data.NodeSearcher    as Match
import           NodeEditor.React.Model.Searcher (Searcher)
import qualified NodeEditor.React.Model.Searcher as Searcher
import           NodeEditor.View.Diff            (DiffT, diffApply)
import           NodeEditor.View.Key             (Key)


entriesNum :: Int
entriesNum = 10

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
    { _key            :: Maybe Key
    , _selected       :: Int
    , _entries        :: [EntryView]
    , _input          :: Text
    , _inputSelection :: Maybe (Int, Int)
    , _targetField    :: Maybe Text
    } deriving (Generic, Show)

makeLenses ''HighlightView
makeLenses ''EntryView
makeLenses ''SearcherView

instance ToJSON HighlightView where
    toEncoding = Lens.toEncoding
    toJSON = Lens.toJSON

instance ToJSON EntryView     where
    toEncoding = Lens.toEncoding
    toJSON = Lens.toJSON

instance ToJSON SearcherView  where
    toEncoding = Lens.toEncoding
    toJSON = Lens.toJSON

instance Convertible (Int, Int) HighlightView where
    convert = uncurry HighlightView

instance Convertible Match EntryView where
    convert m = EntryView
        {- name       -} (m ^. Match.name . to convert)
        {- doc        -} (m ^. Match.doc . to convert)
        {- className  -} (m ^. Match.className . to convert)
        {- highlights -} (m ^. Match.match . to convert)

sliceEntries :: Searcher -> [EntryView]
sliceEntries s = slice es
    where
        es     = s ^. Searcher.hints . to convert
        toDrop = max (s ^. Searcher.selected - 1) 0
        slice  = if length es <= toDrop then id
                 else take entriesNum . drop toDrop

instance Convertible Searcher SearcherView where
    convert s = SearcherView
        {- key            -} (s ^. Searcher.mode . to nodeKey')
        {- selected       -} (s ^. Searcher.selected)
        {- entries        -} (sliceEntries s)
        {- input          -} (s ^. Searcher.inputText)
        {- inputSelection -} (s ^. Searcher.inputSelection)
        {- targetField    -} (s ^. Searcher.targetField)

nodeKey' :: Searcher.Mode -> Maybe Key
nodeKey' = \case
    Searcher.Node nl _ _   -> Just $ convert nl
    Searcher.NodeName nl _ -> Just $ convert nl
    _                      -> Nothing

foreign import javascript safe "atomCallback.getNodeEditorView().setSearcher($1)"
    setSearcher__ :: JSVal -> IO ()

setSearcher :: MonadIO m => Maybe SearcherView -> m ()
setSearcher = liftIO . setSearcher__ <=< toJSONVal

searcherView :: MonadIO m => DiffT (Maybe Searcher) m ()
searcherView = diffApply $ setSearcher . convert
