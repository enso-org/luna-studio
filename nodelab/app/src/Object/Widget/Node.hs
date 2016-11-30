{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module Object.Widget.Node where

import           Data.Aeson                     (ToJSON)
import           Utils.PreludePlus
import           Utils.Vector

import           Data.Map.Lazy                  (Map)
import qualified Data.Text.Lazy                 as Text
import           Data.Time.Clock                (UTCTime)
import qualified Empire.API.Data.Node           as N
import qualified Empire.API.Data.NodeMeta       as NM
import qualified Empire.API.Data.Port           as P
import           Empire.API.Data.PortRef        (AnyPortRef)
import           Empire.API.Graph.Collaboration (ClientId)
import           Object.UITypes
import           Object.Widget
import           Object.Widget.Port             (Port)
import           Reactive.State.Collaboration   (ColorId)



data Elements = Elements { _expressionLabel     :: WidgetId
                         , _portGroup           :: WidgetId
                         , _portControls        :: WidgetId
                         , _inLabelsGroup       :: WidgetId
                         , _outLabelsGroup      :: WidgetId
                         , _expandedGroup       :: WidgetId
                         , _nodeGroup           :: WidgetId
                         , _nameTextBox         :: WidgetId
                         , _valueLabel          :: WidgetId
                         , _visualizationGroup  :: WidgetId
                         , _execTimeLabel       :: WidgetId
                         , _nodeType            :: Maybe WidgetId
                         , _visualizationToggle :: Maybe WidgetId
                         , _codeEditor          :: Maybe WidgetId
                         } deriving (Eq, Show, Generic)

instance Default Elements where
    def = Elements def def def def def def def def def def def def def def

type CollaborationMap = Map ClientId UTCTime
data Collaboration = Collaboration { _touch  :: Map ClientId (UTCTime, ColorId)
                                   , _modify :: CollaborationMap
                                   } deriving (Eq, Show, Generic)

instance Default Collaboration where
    def = Collaboration def def

makeLenses ''Collaboration
instance ToJSON Collaboration

data Node = Node { _nodeId                :: N.NodeId
                 , _ports                 :: Map AnyPortRef Port
                 , _position              :: Position
                 , _zPos                  :: Double
                 , _expression            :: Text
                 , _code                  :: Maybe Text
                 , _name                  :: Text
                 , _value                 :: Text
                 , _tpe                   :: Maybe Text
                 , _isExpanded            :: Bool
                 , _isSelected            :: Bool
                 , _isError               :: Bool
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

makeNode :: N.NodeId -> Position -> Text -> Maybe Text -> Text -> Maybe Text -> Bool -> Node
makeNode nid pos expr code name tpe vis = Node nid def pos 0.0 expr code name "" tpe False False False vis def Nothing False def

fromNode :: N.Node -> Node
fromNode n = let position' = uncurry Vector2 $ n ^. N.nodeMeta ^. NM.position
                 nodeId'   = n ^. N.nodeId
                 name'     = n ^. N.name
                 vis       = n ^. N.nodeMeta . NM.displayResult
                 code      = n ^. N.code
    in
    case n ^. N.nodeType of
        N.ExpressionNode expression ->  makeNode nodeId' position' expression code name' Nothing vis
        N.InputNode inputIx         ->  makeNode nodeId' position' (Text.pack $ "Input " <> show inputIx) code name' (Just tpe) vis where
            tpe = Text.pack $ fromMaybe "?" $ show <$> n ^? N.ports . ix (P.OutPortId P.All) . P.valueType
        N.OutputNode outputIx       ->  makeNode nodeId' position' (Text.pack $ "Output " <> show outputIx) code name' Nothing vis
        N.ModuleNode                ->  makeNode nodeId' position' "Module"    code name' Nothing vis
        N.FunctionNode tpeSig       -> (makeNode nodeId' position' "Function"  code name' Nothing vis) & value .~ (Text.pack $ intercalate " -> " tpeSig)
        N.InputEdge                 ->  makeNode nodeId' position' "Input"     code name' Nothing vis
        N.OutputEdge                ->  makeNode nodeId' position' "Output"    code name' Nothing vis


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
