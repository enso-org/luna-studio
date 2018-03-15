module NodeEditor.View.ExpressionNode where

import           Common.Data.JSON                           (toJSONVal)
import           Common.Prelude
import qualified Control.Lens.Aeson                         as Lens
import           Data.Aeson                                 (ToJSON (toEncoding, toJSON))
import           Data.Convert                               (Convertible (convert))
import qualified Data.HashMap.Strict                        as HashMap
import           LunaStudio.Data.Position                   (toTuple)
import           NodeEditor.React.Model.Node.ExpressionNode (ExpressionNode, ExpressionNodesMap)
import qualified NodeEditor.React.Model.Node.ExpressionNode as ExpressionNode
import           NodeEditor.View.Diff                       (DiffT, diffApply)
import           NodeEditor.View.Port                       (PortView(PortView))


data ExpressionNodeView = ExpressionNodeView
        { _key        :: String
        , _name       :: String
        , _expression :: String
        , _inPorts    :: [PortView]
        , _outPorts   :: [PortView]
        , _position   :: (Double, Double)
        , _expanded   :: Bool
        , _selected   :: Bool
        } deriving (Generic, Show)

makeLenses ''ExpressionNodeView

instance ToJSON ExpressionNodeView where
    toEncoding = Lens.toEncoding
    toJSON     = Lens.toJSON

instance Convertible ExpressionNode ExpressionNodeView where
    convert n = ExpressionNodeView
        {- key        -} (n ^. ExpressionNode.nodeLoc . to show)
        {- name       -} (n ^. ExpressionNode.name . to convert . to (fromMaybe def))
        {- expression -} (n ^. ExpressionNode.expression . to convert)
        {- inPorts    -} (n ^. to ExpressionNode.inPortsList . to convert)
        {- outPorts   -} (n ^. to ExpressionNode.outPortsList . to convert)
        {- position   -} (n ^. ExpressionNode.position . to toTuple)
        {- expanded   -} (n ^. ExpressionNode.mode == ExpressionNode.Expanded ExpressionNode.Controls)
        {- selected   -} (n ^. ExpressionNode.isSelected)

foreign import javascript safe "atomCallback.getNodeEditorView().setNodes($1)"
    setNodes__ :: JSVal -> IO ()

setNodes :: MonadIO m => [ExpressionNodeView] -> m ()
setNodes = liftIO . setNodes__ <=< toJSONVal

expressionNodesView :: MonadIO m => DiffT ExpressionNodesMap m ()
expressionNodesView = diffApply $ setNodes . map convert . HashMap.elems
