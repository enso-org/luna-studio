{-# LANGUAGE Rank2Types #-}

module UI.Widget.Node where

import qualified Data.Map.Lazy                as Map
import           Luna.Studio.Prelude
import           Luna.Studio.Data.Vector

import           Data.Aeson                   (toJSON)
import           GHCJS.Marshal                (toJSVal)
import           GHCJS.Marshal.Pure           (PFromJSVal (..), PToJSVal (..))


import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.Node           as Model
import           Luna.Studio.State.Collaboration (unColorId)

import           UI.Generic                   (whenChanged)
import qualified UI.Registry                  as UIR
import           UI.Widget                    (UIContainer, UIWidget)
import           UI.Widget                    (GenericWidget (..))
import qualified UI.Widget                    as UIT

newtype Node = Node { unNode :: JSVal } deriving (PToJSVal, PFromJSVal)

instance UIWidget    Node
instance UIContainer Node

foreign import javascript safe "new GraphNode(new THREE.Vector2($2, $3), 0, $1)"     create'           :: WidgetId -> Double -> Double -> IO Node
foreign import javascript safe "$1.setZPos($2)"                                      setZPos           :: Node -> Double -> IO ()
foreign import javascript safe "$1.setSelected($2)"                                  setSelected       :: Node -> Bool   -> IO ()
foreign import javascript safe "$1.setError($2)"                                     setError          :: Node -> Bool   -> IO ()
foreign import javascript safe "$1.setHighlight($2)"                                 setHighlight      :: Node -> Bool   -> IO ()
foreign import javascript safe "$1.setCollaboration($2, $3)"                         setCollaboration' :: Node -> Int    -> JSVal -> IO ()


createNode :: WidgetId -> Model.Node -> IO Node
createNode wid model = do
    node <- create' wid (model ^. Model.position . x) (model ^. Model.position . y)
    return node

ifChanged :: (Eq b) => a -> a -> Lens' a b -> IO () -> IO ()
ifChanged old new get action = if (old ^. get) /= (new ^. get) then action
                                                               else return ()

setCollaboration :: Node -> Model.Collaboration -> IO ()
setCollaboration n col = do
    let color | not $ Map.null $ col ^. Model.modify = 2
              | not $ Map.null $ col ^. Model.touch  = 1
              | otherwise                            = 0
        users = unColorId . snd <$> (Map.elems $ col ^. Model.touch)
    users' <- toJSVal $ toJSON users
    setCollaboration' n color users'

instance UIDisplayObject Model.Node where
    createUI parentId wid model = do
        node   <- createNode wid model
        parent <- UIR.lookup parentId :: IO GenericWidget
        UIR.register wid node
        UIT.add node parent

    updateUI wid old model = do
        node <- UIR.lookup wid :: IO Node

        whenChanged old model Model.isSelected    $ setSelected      node $ model ^. Model.isSelected
        --TODO[react]
        -- whenChanged old model Model.isError       $ setError         node $ model ^. Model.isError
        whenChanged old model Model.zPos          $ setZPos          node $ model ^. Model.zPos
        whenChanged old model Model.highlight     $ setHighlight     node $ model ^. Model.highlight
        whenChanged old model Model.collaboration $ setCollaboration node $ model ^. Model.collaboration
