{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module React.Store.Node where

import           Control.DeepSeq    (NFData)
import           Object.Widget.Node (Node)
import           React.Flux
import           Utils.PreludePlus



data Store = Store { _node :: Node }
           deriving (Show, Generic)

makeLenses ''Store

data Action = ModifyNode (Node -> Node)
            deriving (Generic, NFData, Typeable)

instance StoreData Store where
    type StoreAction Store = Action
    transform action store = do
        case action of
            ModifyNode f -> putStrLn "call ModifyNode"
                         >> return (store & node %~ f)

type Ref = ReactStore Store

create :: Node -> IO Ref
create = mkStore . Store
