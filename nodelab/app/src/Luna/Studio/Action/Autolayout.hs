module Luna.Studio.Action.Autolayout where

import           Luna.Studio.Action.Command (Command)
import           Luna.Studio.Prelude
import           Luna.Studio.State.Global   (State)


autolayoutAllNodes :: Command State ()
autolayoutAllNodes = print "autolayout"

autolayoutSelectedNodes :: Command State ()
autolayoutSelectedNodes = print "autolayout selected"
