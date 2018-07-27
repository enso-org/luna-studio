{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.React.Model.Port
    ( module NodeEditor.React.Model.Port
    , module X
    )
    where

import           Common.Prelude                   hiding (set)
import           Data.Convert                     (Convertible (convert))
import qualified Data.Text                        as Text
import           LunaStudio.Data.Angle            (Angle)
import           LunaStudio.Data.Constants        (gridSize)
import           LunaStudio.Data.Geometry         (Radius)
import           LunaStudio.Data.LabeledTree      (LabeledTree (LabeledTree))
import qualified LunaStudio.Data.LabeledTree      as LabeledTree
import           LunaStudio.Data.Port             as X hiding (InPort, OutPort, Port (..), name, portId, state, valueType)
import qualified LunaStudio.Data.Port             as Empire
import           LunaStudio.Data.PortDefault      as X (PortDefault (..))
import           LunaStudio.Data.PortRef          as X (AnyPortRef (InPortRef', OutPortRef'), InPortRef (InPortRef),
                                                        OutPortRef (OutPortRef))
import           LunaStudio.Data.Position         (Position)
import           LunaStudio.Data.TypeRep          (TypeRep (..))
import           NodeEditor.Data.Color            (Color)
import qualified NodeEditor.Data.Color            as Color
import           NodeEditor.React.Model.Constants (lineHeight, nodeRadius)

type IsAlias = Bool
type IsSelf  = Bool
type IsOnly  = Bool

data Mode = Normal
          | Invisible
          | Inactive
          | TypeNotMatched
          | Highlighted
          | Moved Position
          | NameEdit
          deriving (Eq, Show, Typeable, Generic, NFData)

instance Default Mode where
    def = Normal

data Port i = Port
        { _portId    :: i
        , _name      :: Text
        , _valueType :: TypeRep
        , _state     :: PortState
        , _mode      :: Mode
        } deriving (Eq, Functor, Generic, NFData, Show, Typeable)

type InPort     = Port InPortId
type OutPort    = Port OutPortId
type AnyPort    = Port AnyPortId
type EitherPort = Either InPort OutPort

makeLenses ''Port

color :: Getter (Port i) Color
color = valueType . to Color.fromType

isInMode :: Mode -> Port i -> Bool
isInMode m p = case (m, p ^. mode) of
    (Moved _, Moved _) -> True
    (m1, m2)           -> m1 == m2

isInNormalMode :: Port i -> Bool
isInNormalMode = isInMode Normal

isInvisible :: Port i -> Bool
isInvisible = isInMode Invisible

ensureVisibility :: Mode -> Mode
ensureVisibility Invisible = Normal
ensureVisibility m = m

isHighlighted :: Port i -> Bool
isHighlighted = isInMode Highlighted

isInMovedMode :: Port i -> Bool
isInMovedMode = isInMode (Moved def)

isInNameEditMode :: Port i -> Bool
isInNameEditMode = isInMode NameEdit

getPositionInSidebar :: Port i -> Maybe Position
getPositionInSidebar p = case p ^. mode of
    Moved pos -> Just pos
    _         -> Nothing

visibleOutPorts :: OutPortTree (Port i) -> [Port i]
visibleOutPorts (LabeledTree (OutPorts []) p) = if Text.null (p ^. name) || not (isUpper . Text.head $ p ^. name) then [p] else []
visibleOutPorts (LabeledTree (OutPorts ps) _) = concatMap visibleOutPorts ps

visibleInPorts :: InPortTree (Port i) -> [Port i]
visibleInPorts root@(LabeledTree (InPorts maySelf _ args') _) = findSelfAndOrHead <> map (view LabeledTree.value) args' where
    h                                                = findHead root
    findSelfAndOrHead                                = (if h ^. state == Connected then [h] else []) <> maybeToList (findSelf <$> maySelf)
    findHead (LabeledTree (InPorts _ mayHead' _) p') = if p' ^. state == Connected then p' else maybe p' findHead mayHead'
    findSelf (LabeledTree (InPorts maySelf' _ _) p') = if p' ^. state == Connected then p' else maybe p' findSelf maySelf'


instance Convertible InPort  AnyPort    where convert = fmap InPortId'
instance Convertible OutPort AnyPort    where convert = fmap OutPortId'
instance Convertible InPort  EitherPort where convert = convert . fmap InPortId'
instance Convertible OutPort EitherPort where convert = convert . fmap OutPortId'

instance Convertible AnyPort EitherPort where
    convert (Port (InPortId'  i) n nt s m) = Left  $ Port i n nt s m
    convert (Port (OutPortId' i) n nt s m) = Right $ Port i n nt s m

instance Convertible EitherPort AnyPort where
    convert (Left  port) = InPortId'  <$> port
    convert (Right port) = OutPortId' <$> port

instance Convertible (Empire.Port i) (Port i) where
    convert p = Port
        {- portId   -} (p ^. Empire.portId)
        {- name     -} (p ^. Empire.name)
        {- nodeType -} (p ^. Empire.valueType)
        {- state    -} (p ^. Empire.state)
        {- mode     -} Normal

instance Convertible (Port i) (Empire.Port i) where
    convert p = Empire.Port
        {- portId   -} (p ^. portId)
        {- name     -} (p ^. name)
        {- nodeType -} (p ^. valueType)
        {- state    -} (p ^. state)

instance Default (InPortTree InPort) where
    def = convert (def :: InPortTree Empire.InPort)

instance Default (OutPortTree OutPort) where
    def = convert (def :: OutPortTree Empire.OutPort)

portGap :: Radius -> Angle
portGap r = 0.2 * nodeRadius / r -- to avoid gap narrowing

portAngle :: Int -> Angle
portAngle numOfPorts = pi / fromIntegral numOfPorts

portAngleStart :: Bool -> Int -> Int -> Radius -> Angle
portAngleStart trimmed portNumber numOfPorts r = n * t + trim
    where n    = fromIntegral portNumber
          trim = if trimmed then (portGap r)/2 else 0
          t    = portAngle numOfPorts

portAngleStop :: Bool -> Int -> Int -> Radius -> Angle
portAngleStop trimmed portNumber numOfPorts r = n * t + t - trim
    where n    = fromIntegral portNumber
          trim = if trimmed then (portGap r)/2 else 0
          t    = portAngle numOfPorts

argumentConstructorNumber :: Int -> Int
argumentConstructorNumber = max 1

argumentConstructorOffsetY :: Int -> Double
argumentConstructorOffsetY numOfPorts = (fromIntegral $ argumentConstructorNumber numOfPorts) * gridSize
