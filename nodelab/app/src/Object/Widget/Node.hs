{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
module Object.Widget.Node where

import           Control.Arrow
import           Data.Aeson                        (ToJSON)
import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude               hiding (set)

import           Data.Map.Lazy                     (Map)
import qualified Data.Map.Lazy                     as Map
import qualified Data.Text.Lazy                    as Text
import           Data.Time.Clock                   (UTCTime)
import qualified Empire.API.Data.Node              as N
import qualified Empire.API.Data.NodeMeta          as NM
import           Empire.API.Data.Port              (OutPort (..), PortId (..))
import qualified Empire.API.Data.Port              as P
import           Empire.API.Data.PortRef           (AnyPortRef, toAnyPortRef)
import           Empire.API.Graph.Collaboration    (ClientId)
import           Empire.API.Graph.NodeResultUpdate (NodeValue)
import           Luna.Studio.Data.Color            (colorPort)
import           Luna.Studio.State.Collaboration   (ColorId)
import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Port                as PortModel



data Elements = Elements { _portGroup           :: WidgetId
                         , _portControls        :: WidgetId
                         , _inLabelsGroup       :: WidgetId
                         , _outLabelsGroup      :: WidgetId
                         , _expandedGroup       :: WidgetId
                         , _nodeGroup           :: WidgetId
                         , _nameTextBox         :: WidgetId
                         , _visualizationGroup  :: WidgetId
                         , _execTimeLabel       :: WidgetId
                         , _nodeType            :: Maybe WidgetId
                         , _codeEditor          :: Maybe WidgetId
                         } deriving (Eq, Show, Generic)

instance Default Elements where
    def = Elements def def def def def def def def def def def

type CollaborationMap = Map ClientId UTCTime
data Collaboration = Collaboration { _touch  :: Map ClientId (UTCTime, ColorId)
                                   , _modify :: CollaborationMap
                                   } deriving (Eq, Show, Generic)

instance Default Collaboration where
    def = Collaboration def def

makeLenses ''Collaboration
instance ToJSON Collaboration

data Node = Node { _nodeId                :: N.NodeId
                 , _ports                 :: Map AnyPortRef PortModel.Port
                 , _position              :: Position
                 , _zPos                  :: Double
                 , _expression            :: Text
                 , _code                  :: Maybe Text
                 , _name                  :: Text
                 , _value                 :: Maybe NodeValue
                 , _tpe                   :: Maybe Text
                 , _isExpanded            :: Bool
                 , _isSelected            :: Bool
                 , _visualizationsEnabled :: Bool
                 , _collaboration         :: Collaboration
                 , _execTime              :: Maybe Integer
                 , _highlight             :: Bool
                 , _elements              :: Elements
                 } deriving (Eq, Show, Typeable, Generic)


makeLenses ''Node
instance ToJSON Node

makeLenses ''Elements
instance ToJSON Elements

makeNode :: N.NodeId -> Map AnyPortRef PortModel.Port -> Position -> Text -> Maybe Text -> Text -> Maybe Text -> Bool -> Node
makeNode nid ports' pos expr code' name' tpe' vis = Node nid ports' pos 0.0 expr code' name' Nothing tpe' False False vis def Nothing False def

makePorts :: N.Node -> [PortModel.Port]
makePorts node = makePort <$> ports' where
    nodeId'  = node ^. N.nodeId
    makePort port = PortModel.Port portRef portId (colorPort port) False where
        portId  = port ^. P.portId
        portRef = toAnyPortRef nodeId' portId
    ports'  = Map.elems $ node ^. N.ports

fromNode :: N.Node -> Node
fromNode n = let position' = uncurry Vector2 $ n ^. N.nodeMeta ^. NM.position
                 nodeId'   = n ^. N.nodeId
                 name'     = n ^. N.name
                 vis       = n ^. N.nodeMeta . NM.displayResult
                 code'     = n ^. N.code
                 ports'    = Map.fromList $ map (view PortModel.portRef &&& id) $ makePorts n
    in
    case n ^. N.nodeType of
        N.ExpressionNode expression' ->  makeNode nodeId' ports' position' expression' code' name' Nothing vis
        N.InputNode inputIx          ->  makeNode nodeId' ports' position' (Text.pack $ "Input " <> show inputIx) code' name' (Just tpe') vis where
            tpe' = Text.pack $ fromMaybe "?" $ show <$> n ^? N.ports . ix (OutPortId All) . P.valueType
        N.OutputNode outputIx        ->  makeNode nodeId' ports' position' (Text.pack $ "Output " <> show outputIx) code' name' Nothing vis
        N.ModuleNode                 ->  makeNode nodeId' ports' position' "Module"    code' name' Nothing vis
        N.FunctionNode tpeSig        -> (makeNode nodeId' ports' position' "Function"  code' name' Nothing vis) -- & value .~ (Text.pack $ intercalate " -> " tpeSig) --TODO[react]
        N.InputEdge                  ->  makeNode nodeId' ports' position' "Input"     code' name' Nothing vis
        N.OutputEdge                 ->  makeNode nodeId' ports' position' "Output"    code' name' Nothing vis


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
