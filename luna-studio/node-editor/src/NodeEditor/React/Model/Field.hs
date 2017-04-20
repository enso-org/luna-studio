module NodeEditor.React.Model.Field where

import           Common.Prelude
import           NodeEditor.React.Model.App (App)
import           NodeEditor.React.Store     (Ref)
import NodeEditor.Event.UI (UIEvent)


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
