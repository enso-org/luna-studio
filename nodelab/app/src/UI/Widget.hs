module UI.Widget where

import           Luna.Studio.Prelude

import           GHCJS.Marshal.Pure (PFromJSVal (..), PToJSVal (..))

newtype Container = Container { unContainer :: JSVal } deriving (PFromJSVal, PToJSVal)
newtype Mesh      = Mesh      { unMesh :: JSVal } deriving (PFromJSVal, PToJSVal)

class (PFromJSVal a, PToJSVal a) => UIWidget a
class (UIWidget a) => UIContainer a

foreign import javascript safe "$1.mesh"      getMesh'      :: JSVal -> IO Mesh
foreign import javascript safe "$1.container" getContainer' :: JSVal -> IO Container
foreign import javascript safe "$2.add($1)"   add'          :: Mesh  -> Container -> IO ()

newtype GenericWidget = GenericWidget { unGenericWidget :: JSVal } deriving (PToJSVal, PFromJSVal)

instance UIWidget    GenericWidget
instance UIContainer GenericWidget

getMesh      :: UIWidget a => a -> IO Mesh
getMesh = getMesh' . pToJSVal

getContainer :: UIContainer a => a -> IO Container
getContainer = getContainer' . pToJSVal

add :: (UIWidget a, UIContainer b) => a -> b -> IO ()
add widget container = do
    mesh           <- getMesh widget
    containerMesh  <- getContainer container
    add' mesh containerMesh
