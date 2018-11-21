{-# LANGUAGE UndecidableInstances #-}

module LunaStudio.API.Response where

import Prologue

{-import qualified LunaStudio.API.Atom.GetBuffer           as GetBuffer-}
{-import qualified LunaStudio.API.Atom.Substitute          as Substitute-}
{-import qualified LunaStudio.API.Control.Interpreter      as Interpreter-}
import qualified LunaStudio.API.Graph.AddConnection      as AddConnection
{-import qualified LunaStudio.API.Graph.AddImports         as AddImports-}
import qualified LunaStudio.API.Graph.AddNode            as AddNode
{-import qualified LunaStudio.API.Graph.AddPort            as AddPort-}
{-import qualified LunaStudio.API.Graph.AddSubgraph        as AddSubgraph-}
{-import qualified LunaStudio.API.Graph.AutolayoutNodes    as AutolayoutNodes-}
{-import qualified LunaStudio.API.Graph.CollapseToFunction as CollapseToFunction-}
{-import qualified LunaStudio.API.Graph.Copy               as Copy-}
{-import qualified LunaStudio.API.Graph.DumpGraphViz       as DumpGraphViz-}
{-import qualified LunaStudio.API.Graph.GetProgram         as GetProgram-}
{-import qualified LunaStudio.API.Graph.GetSubgraphs       as GetSubgraphs-}
{-import qualified LunaStudio.API.Graph.MovePort           as MovePort-}
{-import qualified LunaStudio.API.Graph.Paste              as Paste-}
{-import qualified LunaStudio.API.Graph.RemoveConnection   as RemoveConnection-}
import qualified LunaStudio.API.Graph.RemoveNodes        as RemoveNodes
{-import qualified LunaStudio.API.Graph.RemovePort         as RemovePort-}
{-import qualified LunaStudio.API.Graph.RenameNode         as RenameNode-}
{-import qualified LunaStudio.API.Graph.RenamePort         as RenamePort-}
{-import qualified LunaStudio.API.Graph.SaveSettings       as SaveSettings-}
{-import qualified LunaStudio.API.Graph.SearchNodes        as SearchNodes-}
{-import qualified LunaStudio.API.Graph.SetCode            as SetCode-}
import qualified LunaStudio.API.Graph.SetNodeExpression  as SetNodeExpression
{-import qualified LunaStudio.API.Graph.SetNodesMeta       as SetNodesMeta-}
{-import qualified LunaStudio.API.Graph.SetPortDefault     as SetPortDefault-}
{-import qualified LunaStudio.API.Graph.TypeCheck          as TypeCheck-}

import qualified LunaStudio.API.Topic                    as Topic

import Control.Lens           (makePrisms)
import Data.Aeson.Types       (ToJSON)
import Data.Binary            (Binary)
import Data.UUID.Types        (UUID)
import LunaStudio.API.Request (Request (..))
import LunaStudio.API.Topic   (MessageTopic)
import LunaStudio.Data.Diff   (Diff)
import LunaStudio.Data.Error  (Error, LunaError)


data Status a
    = Ok    { _resultData :: a }
    | Error { _lunaError  :: Error LunaError }
    deriving (Eq, Generic, Show)

makeLenses ''Status
makePrisms ''Status

instance Binary a => Binary (Status a)
instance NFData a => NFData (Status a)
instance ToJSON a => ToJSON (Status a)

data Response req inv res = Response
    { _requestId :: UUID
    , _guiID     :: Maybe UUID
    , _request   :: req
    , _inverse   :: Status inv
    , _status    :: Status res
    } deriving (Eq, Generic, Show)

type SimpleResponse req inv = Response req inv ()

type family InverseOf a
type family ResultOf  a

type ResponseOf req = Response req (InverseOf req) (ResultOf req)

instance (Topic.MessageTopic req, InverseOf req ~ inv, ResultOf req ~ res) => Topic.MessageTopic (Response req inv res) where
    topic = Topic.topic @req <> Topic.response

type ResponseResult req inv res =
    ( MessageTopic (Request req)
    , MessageTopic (Response req inv res)
    , Binary req
    , Binary inv
    , Binary res
    , res ~ ResultOf req
    , inv ~ InverseOf req
    )

result :: ResponseResult req inv res => Request req -> inv -> res -> Response req inv res
result (Request uuid guiID req) inv payload
    = Response uuid guiID req (Ok inv) (Ok payload)

error :: ResponseResult req inv res => Request req -> Status inv -> Error LunaError
    -> Response req inv res
error  (Request uuid guiID req) inv err
    = Response uuid guiID req inv (Error err)

ok :: (ResponseResult req inv (), MessageTopic (Response req inv ())) 
   => Request req -> inv -> Response req inv ()
ok (Request uuid guiID req) inv = Response uuid guiID req (Ok inv) (Ok ())

makeLenses ''Response

instance (Binary req, Binary res, Binary inv) => Binary (Response req inv res)
instance (NFData req, NFData res, NFData inv) => NFData (Response req inv res)
instance (ToJSON req, ToJSON res, ToJSON inv) => ToJSON (Response req inv res)

-----------------------
-- === Instances === --
-----------------------

type instance InverseOf AddConnection.Request = SetNodeExpression.Request
type instance ResultOf  AddConnection.Request = Diff

type instance InverseOf AddNode.Request = RemoveNodes.Request
type instance ResultOf  AddNode.Request = Diff

type instance InverseOf RemoveNodes.Request = RemoveNodes.Inverse
type instance ResultOf  RemoveNodes.Request = Diff

type instance InverseOf SetNodeExpression.Request = SetNodeExpression.Request
type instance ResultOf  SetNodeExpression.Request = Diff
