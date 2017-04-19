module Node.Editor.React.Model.Field where

import           Luna.Prelude
import           Node.Editor.React.Model.App (App)
import           Node.Editor.React.Store     (Ref)
import Node.Editor.Event.UI (UIEvent)


data Field = Field { _ref :: Ref App
                   , _content  :: Text
                   , _onAccept :: Maybe (Text -> UIEvent)
                   , _onCancel :: Maybe (Text -> UIEvent)
                   , _onEdit   :: Maybe (Text -> UIEvent)
                   }

makeLenses ''Field

instance Eq Field where a == b = a ^. content == b ^. content

mk :: Ref App -> Text -> Field
mk r c = Field r c def def def
