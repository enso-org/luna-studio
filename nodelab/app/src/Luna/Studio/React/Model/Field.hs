module Luna.Studio.React.Model.Field where

import           Luna.Studio.Prelude
import           Luna.Studio.React.Model.App (App)
import           Luna.Studio.React.Store     (Ref)
import Luna.Studio.Event.UI (UIEvent)


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
