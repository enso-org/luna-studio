module NodeEditor.View.ExpressionNode where

import           Common.Data.JSON                           (toJSONVal)
import           Common.Prelude
import qualified Control.Lens.Aeson                         as Lens
import           Data.Aeson                                 (FromJSON, ToJSON (toEncoding, toJSON))
import           Data.Convert                               (Convertible (convert))
import qualified Data.HashMap.Strict                        as HashMap
import           LunaStudio.Data.PortRef                    (dstPortId)
import           LunaStudio.Data.Position                   (toTuple)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, ExpressionNodesMap)
import qualified NodeEditor.React.Model.Node.ExpressionNode as ExpressionNode
import           NodeEditor.View.Diff                       (DiffT, diffApply, diffConvert, diffHashMap)
import           NodeEditor.View.Port                       (PortView (PortView))
import           NodeEditor.View.Key                        (Key)
import           NodeEditor.View.PortControl                (PortControlsView)


data ValueContent
    = Visualization
    | ShortValue (Maybe String)
    deriving (Eq, Generic, Show)

data ValueView
    = Error ValueContent
    | Value ValueContent
    deriving (Eq, Generic, Show)

data ExpressionNodeView = ExpressionNodeView
    { _key        :: Key
    , _name       :: String
    , _expression :: String
    , _value      :: ValueView
    , _inPorts    :: [PortView]
    , _outPorts   :: [PortView]
    , _controls   :: [PortControlsView]
    , _newPortKey :: Key
    , _position   :: (Double, Double)
    , _expanded   :: Bool
    , _selected   :: Bool
    } deriving (Eq, Generic, Show)

makeLenses ''ExpressionNodeView
makePrisms ''ValueContent
makePrisms ''ValueView


instance FromJSON ValueView
instance FromJSON ValueContent
instance FromJSON ExpressionNodeView
instance NFData   ValueView
instance NFData   ValueContent
instance NFData   ExpressionNodeView
instance ToJSON   ValueView where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON
instance ToJSON   ValueContent where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON
instance ToJSON   ExpressionNodeView where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON

instance Convertible ExpressionNode ValueView where
    convert n = let
            visActive = if ExpressionNode.returnsError n
                then n ^. ExpressionNode.errorVisEnabled
                else n ^. ExpressionNode.visEnabled
            valContent = if visActive
                then Visualization
                else ShortValue $ convert <$> ExpressionNode.getValue n
        in if ExpressionNode.returnsError n
            then Error valContent
            else Value valContent


instance Convertible ExpressionNode ExpressionNodeView where
    convert n = ExpressionNodeView
        {- key        -} (n ^. ExpressionNode.nodeLoc . to convert)
        {- name       -}
            (n ^. ExpressionNode.name . to convert . to (fromMaybe def))
        {- expression -} (n ^. ExpressionNode.expression . to convert)
        {- value      -} (convert n)
        {- inPorts    -} (n ^. to ExpressionNode.inPortsList . to convert)
        {- outPorts   -} (n ^. to ExpressionNode.outPortsList . to convert)
        {- controls   -} (convert $
            if ExpressionNode.isExpanded n
            && null (ExpressionNode.inPortsList n)
                then maybeToList $ n ^? ExpressionNode.inPortAt []
                else ExpressionNode.inPortsList n)
        {- newPortKey -} (n ^. to ExpressionNode.argumentConstructorRef . dstPortId . to convert)
        {- position   -} (n ^. ExpressionNode.position . to toTuple)
        {- expanded   -} (n ^. ExpressionNode.mode == ExpressionNode.Expanded ExpressionNode.Controls)
        {- selected   -} (n ^. ExpressionNode.isSelected)

foreign import javascript safe "atomCallback.getNodeEditorView().setNode($1)"
    setNode__ :: JSVal -> IO ()

foreign import javascript safe "atomCallback.getNodeEditorView().unsetNode($1)"
    unsetNode__ :: JSVal -> IO ()

setNode :: MonadIO m => ExpressionNodeView -> m ()
setNode = liftIO . setNode__ <=< toJSONVal

unsetNode :: MonadIO m => ExpressionNodeView -> m ()
unsetNode = liftIO . unsetNode__ <=< toJSONVal

expressionNodeView :: MonadIO m => DiffT ExpressionNodeView m ()
expressionNodeView = diffApply setNode

expressionNodesView :: MonadIO m => DiffT ExpressionNodesMap m ()
expressionNodesView = diffHashMap
    (diffConvert expressionNodeView)
    (setNode . convert)
    (unsetNode . convert)
