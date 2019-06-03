module LunaStudio.Data.GraphLocation where

import Prologue hiding ((|>))

import qualified Control.Lens.Aeson as Lens
import qualified Data.Binary        as Binary
import qualified Path

import Data.Aeson.Types           (FromJSON (..), ToJSON (..))
import Data.Binary                (Binary)
import LunaStudio.Data.Breadcrumb (Breadcrumb (Breadcrumb),
                                   BreadcrumbItem (Arg, Definition, Lambda))
import LunaStudio.Data.NodeId     (NodeId)
import Path                       (Path, Rel, File)

instance Binary (Path Rel File) where
    put = Binary.put . Path.toFilePath
    get = do
        filepath <- Binary.get
        case Path.parseRelFile filepath of
            Nothing -> fail "Not a relative filepath"
            Just p  -> pure p

data GraphLocation = GraphLocation
    { _filePath   :: Path Rel File
    , _breadcrumb :: Breadcrumb BreadcrumbItem
    } deriving (Eq, Generic, Show)

makeLenses ''GraphLocation

instance Binary   GraphLocation
instance NFData   GraphLocation
instance FromJSON GraphLocation where parseJSON = Lens.parse
instance ToJSON   GraphLocation where
    toJSON     = Lens.toJSON
    toEncoding = Lens.toEncoding

class Location a where
    top :: a -> GraphLocation

instance Location (Path Rel File) where
    top fp = GraphLocation fp def

instance Location GraphLocation where
    top gl = gl & breadcrumb .~ def

isTop :: GraphLocation -> Bool
isTop gl = null $ gl ^. breadcrumb

infixl 5 |>
(|>) :: GraphLocation -> BreadcrumbItem -> GraphLocation
(|>) = appendBreadcrumbItem

appendBreadcrumbItem :: GraphLocation -> BreadcrumbItem -> GraphLocation
appendBreadcrumbItem (GraphLocation file bc) item
    = GraphLocation file . coerce . (<> [item]) $ coerce bc

infixl 5 |>|
(|>|) :: GraphLocation -> NodeId -> GraphLocation
(|>|) = appendLambda

appendLambda :: GraphLocation -> NodeId -> GraphLocation
appendLambda gl nid = gl |> Lambda nid

infixl 5 |>-
(|>-) :: GraphLocation -> (NodeId, Int) -> GraphLocation
(|>-) = appendArg

appendArg :: GraphLocation -> (NodeId, Int) -> GraphLocation
appendArg gl it = gl |> uncurry Arg it

infixl 5 |>=
(|>=) :: GraphLocation -> NodeId -> GraphLocation
(|>=) = appendDefinition

appendDefinition :: GraphLocation -> NodeId -> GraphLocation
appendDefinition gl it = gl |> Definition it
