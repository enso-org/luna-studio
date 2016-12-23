module Reactive.Plugins.Core.Action.Undo where


import           Data.Aeson                        (decode, encode)
import           Data.ByteString.Lazy.Char8        (unpack)
import qualified Data.HashMap.Strict               as HashMap
import qualified Data.Set                          as Set
import           Data.Text.Lazy.Encoding           (encodeUtf8)
import qualified Empire.API.Data.Connection        as Connection
import           Empire.API.Data.Node              (Node)
import qualified Empire.API.Data.Node              as Node
import qualified Empire.API.Data.NodeMeta          as NodeMeta
import qualified Empire.API.Data.PortRef           as PortRef
import qualified Event.Clipboard                   as Clipboard
import           Event.Event                       (Event (..))
import           Event.Keyboard                    (KeyMods (..))
import qualified Event.Keyboard                    as Keyboard
import qualified Object.Widget                     as Widget
import qualified Object.Widget.Node                as UINode
import           Reactive.Commands.Batch           (requestRedo, requestUndo)
import           Reactive.Commands.Command         (Command)
import           Reactive.State.Global             (State)
import           Utils.PreludePlus
import           Utils.Vector                      (Vector2 (..))


toAction :: Event -> Maybe (Command State ())
toAction (Keyboard _ (Keyboard.Event Keyboard.Down 'Z' (KeyMods False True False False))) = Just $ reqUndo
toAction (Keyboard _ (Keyboard.Event Keyboard.Down 'Y' (KeyMods False True False False))) = Just $ reqRedo
toAction _ = Nothing

reqUndo :: Command State ()
reqUndo = requestUndo

reqRedo :: Command State ()
reqRedo = requestRedo
