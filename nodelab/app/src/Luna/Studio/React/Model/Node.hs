{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
module Luna.Studio.React.Model.Node (
    module Luna.Studio.React.Model.Node,
    NodeAPI.NodeId
) where

import           Control.Arrow
import           Data.Aeson                        (ToJSON)
import           Data.Map.Lazy                     (Map)
import qualified Data.Map.Lazy                     as Map
import           Data.Time.Clock                   (UTCTime)

import           Data.Position                     (Position (Position), Vector2 (Vector2))
import qualified Empire.API.Data.Node              as NodeAPI
import qualified Empire.API.Data.NodeMeta          as MetaAPI
import           Empire.API.Data.Port              (OutPort (..), PortId (..))
import qualified Empire.API.Data.Port              as PortAPI
import           Empire.API.Data.PortRef           (AnyPortRef)
import           Empire.API.Data.PortRef           (portId')
import           Empire.API.Graph.Collaboration    (ClientId)
import           Empire.API.Graph.NodeResultUpdate (NodeValue)
import           Luna.Studio.Prelude               hiding (set)
import           Luna.Studio.React.Model.Port      (Port)
import qualified Luna.Studio.React.Model.Port      as Port
import           Luna.Studio.State.Collaboration   (ColorId)



data Node = Node { _nodeId                :: NodeAPI.NodeId
                 , _ports                 :: Map AnyPortRef Port
                 , _position              :: Position
                 , _zPos                  :: Int
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
                 } deriving (Eq, Show, Typeable, Generic, NFData)

type CollaborationMap = Map ClientId UTCTime
data Collaboration = Collaboration { _touch  :: Map ClientId (UTCTime, ColorId)
                                   , _modify :: CollaborationMap
                                   } deriving (Eq, Show, Generic, NFData)
makeLenses ''Node
makeLenses ''Collaboration

isLiteral :: Getter Node Bool
isLiteral = to $ isLiteral' where
    isLiteral' node = not $ any isIn' portIds where
        portIds = map portId' $ Map.keys $ node ^. ports
        isIn' :: PortId -> Bool
        isIn' (OutPortId _) = False
        isIn' (InPortId  _) = True

instance ToJSON Node
instance ToJSON Collaboration

instance Default Collaboration where
    def = Collaboration def def

makeNode :: NodeAPI.NodeId -> Map AnyPortRef Port -> Position -> Text -> Maybe Text -> Text -> Maybe Text -> Bool -> Node
makeNode nid ports' pos expr code' name' tpe' vis = Node nid ports' pos def expr code' name' def def tpe' False False vis def Nothing

makePorts :: NodeAPI.Node -> [Port]
makePorts node = Port.fromPorts (node ^. NodeAPI.nodeId) (Map.elems $ node ^. NodeAPI.ports)

fromNode :: NodeAPI.Node -> Node
fromNode n = let position' = Position (uncurry Vector2 $ n ^. NodeAPI.nodeMeta ^. MetaAPI.position)
                 nodeId'   = n ^. NodeAPI.nodeId
                 name'     = n ^. NodeAPI.name
                 vis       = n ^. NodeAPI.nodeMeta . MetaAPI.displayResult
                 code'     = n ^. NodeAPI.code
                 ports'    = Map.fromList $ map (view Port.portRef &&& id) $ makePorts n
    in
    case n ^. NodeAPI.nodeType of
        NodeAPI.ExpressionNode expression' ->  makeNode nodeId' ports' position' expression' code' name' Nothing vis
        NodeAPI.InputNode inputIx          ->  makeNode nodeId' ports' position' (convert $ "Input " <> show inputIx) code' name' (Just tpe') vis where
            tpe' = convert $ fromMaybe "?" $ show <$> n ^? NodeAPI.ports . ix (OutPortId All) . PortAPI.valueType
        NodeAPI.OutputNode outputIx        ->  makeNode nodeId' ports' position' (convert $ "Output " <> show outputIx) code' name' Nothing vis
        NodeAPI.ModuleNode                 ->  makeNode nodeId' ports' position' "Module"    code' name' Nothing vis
        NodeAPI.FunctionNode tpeSig        -> (makeNode nodeId' ports' position' "Function"  code' name' Nothing vis) -- & value .~ (convert $ intercalate " -> " tpeSig) --TODO[react]
        NodeAPI.InputEdge                  ->  makeNode nodeId' ports' position' "Input"     code' name' Nothing vis
        NodeAPI.OutputEdge                 ->  makeNode nodeId' ports' position' "Output"    code' name' Nothing vis
