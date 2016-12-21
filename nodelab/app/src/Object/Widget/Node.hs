{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
module Object.Widget.Node where

import           Control.Arrow
import           Data.Aeson                        (ToJSON)
import           Luna.Studio.Data.Vector           (Position, Vector2 (Vector2))
import           Luna.Studio.Prelude               hiding (set)

import           Data.Map.Lazy                     (Map)
import qualified Data.Map.Lazy                     as Map
import qualified Data.Text.Lazy                    as Text
import           Data.Time.Clock                   (UTCTime)
import qualified Empire.API.Data.Node              as NodeAPI
import qualified Empire.API.Data.NodeMeta          as MetaAPI
import           Empire.API.Data.Port              (OutPort (..), PortId (..))
import qualified Empire.API.Data.Port              as PortAPI
import           Empire.API.Data.PortRef           (AnyPortRef)
import           Empire.API.Graph.Collaboration    (ClientId)
import           Empire.API.Graph.NodeResultUpdate (NodeValue)
import           Luna.Studio.State.Collaboration   (ColorId)
import           Object.UITypes
import           Object.Widget
import           Object.Widget.Port                (Port)
import qualified Object.Widget.Port                as Port



data Elements = Elements { _portControls        :: WidgetId
                         , _inLabelsGroup       :: WidgetId
                         , _outLabelsGroup      :: WidgetId
                         , _expandedGroup       :: WidgetId
                         , _nodeGroup           :: WidgetId
                         , _nodeType            :: Maybe WidgetId
                         , _codeEditor          :: Maybe WidgetId
                         } deriving (Eq, Show, Generic)

instance Default Elements where
    def = Elements def def def def def def def

type CollaborationMap = Map ClientId UTCTime
data Collaboration = Collaboration { _touch  :: Map ClientId (UTCTime, ColorId)
                                   , _modify :: CollaborationMap
                                   } deriving (Eq, Show, Generic)

instance Default Collaboration where
    def = Collaboration def def

makeLenses ''Collaboration
instance ToJSON Collaboration

data Node = Node { _nodeId                :: NodeAPI.NodeId
                 , _ports                 :: Map AnyPortRef Port
                 , _position              :: Position
                 , _zPos                  :: Double
                 , _expression            :: Text
                 , _code                  :: Maybe Text
                 , _name                  :: Text
                 , _nameEdit              :: Maybe Text
                 , _value                 :: Maybe NodeValue
                 , _tpe                   :: Maybe Text
                 , _isExpanded            :: Bool
                 , _isSelected            :: Bool
                 , _visualizationsEnabled :: Bool
                 , _collaboration         :: Collaboration
                 , _execTime              :: Maybe Integer
                 , _elements              :: Elements
                 } deriving (Eq, Show, Typeable, Generic)


makeLenses ''Node
instance ToJSON Node

makeLenses ''Elements
instance ToJSON Elements

makeNode :: NodeAPI.NodeId -> Map AnyPortRef Port -> Position -> Text -> Maybe Text -> Text -> Maybe Text -> Bool -> Node
makeNode nid ports' pos expr code' name' tpe' vis = Node nid ports' pos 0.0 expr code' name' def def tpe' False False vis def Nothing def

makePorts :: NodeAPI.Node -> [Port]
makePorts node = Port.fromPorts (node ^. NodeAPI.nodeId) (Map.elems $ node ^. NodeAPI.ports)

fromNode :: NodeAPI.Node -> Node
fromNode n = let position' = uncurry Vector2 $ n ^. NodeAPI.nodeMeta ^. MetaAPI.position
                 nodeId'   = n ^. NodeAPI.nodeId
                 name'     = n ^. NodeAPI.name
                 vis       = n ^. NodeAPI.nodeMeta . MetaAPI.displayResult
                 code'     = n ^. NodeAPI.code
                 ports'    = Map.fromList $ map (view Port.portRef &&& id) $ makePorts n
    in
    case n ^. NodeAPI.nodeType of
        NodeAPI.ExpressionNode expression' ->  makeNode nodeId' ports' position' expression' code' name' Nothing vis
        NodeAPI.InputNode inputIx          ->  makeNode nodeId' ports' position' (Text.pack $ "Input " <> show inputIx) code' name' (Just tpe') vis where
            tpe' = Text.pack $ fromMaybe "?" $ show <$> n ^? NodeAPI.ports . ix (OutPortId All) . PortAPI.valueType
        NodeAPI.OutputNode outputIx        ->  makeNode nodeId' ports' position' (Text.pack $ "Output " <> show outputIx) code' name' Nothing vis
        NodeAPI.ModuleNode                 ->  makeNode nodeId' ports' position' "Module"    code' name' Nothing vis
        NodeAPI.FunctionNode tpeSig        -> (makeNode nodeId' ports' position' "Function"  code' name' Nothing vis) -- & value .~ (Text.pack $ intercalate " -> " tpeSig) --TODO[react]
        NodeAPI.InputEdge                  ->  makeNode nodeId' ports' position' "Input"     code' name' Nothing vis
        NodeAPI.OutputEdge                 ->  makeNode nodeId' ports' position' "Output"    code' name' Nothing vis


instance IsDisplayObject Node where
    widgetPosition = position
    widgetSize     = lens get set where
        get _      = Vector2 60.0 60.0
        set w _    = w
    widgetVisible  = to $ const True

data PendingNode = PendingNode { _pendingExpression :: Text
                               , _pendingPosition   :: Position
                               } deriving (Eq, Show, Typeable, Generic)

makeLenses ''PendingNode
instance ToJSON PendingNode

instance IsDisplayObject PendingNode where
    widgetPosition = pendingPosition
    widgetSize     = lens get set where
        get _      = Vector2 60.0 60.0
        set w _    = w
    widgetVisible  = to $ const True
