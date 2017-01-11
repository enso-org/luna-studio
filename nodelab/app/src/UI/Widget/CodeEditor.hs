{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module UI.Widget.CodeEditor where

import           Luna.Studio.Data.Vector
import           Luna.Studio.Prelude

import qualified Data.JSString            ()
import           Data.JSString.Text       (textToJSString)
import           GHCJS.Marshal.Pure       (PFromJSVal (..), PToJSVal (..))

import           Object.UITypes
import           Object.Widget
import qualified Object.Widget.CodeEditor as Model

import           UI.Generic               (whenChanged)
import qualified UI.Generic               as UI
import qualified UI.Registry              as UI
import           UI.Widget                (UIWidget)
import qualified UI.Widget                as Widget


newtype CodeEditor = CodeEditor JSVal deriving (PToJSVal, PFromJSVal)

instance UIWidget CodeEditor

foreign import javascript safe "new CodeEditor($1, $2, $3)" create'        :: Int        -> Double -> Double -> IO CodeEditor
foreign import javascript safe "$1.setCode($2)"             setCode'       :: CodeEditor -> JSString         -> IO ()

create :: WidgetId -> Model.CodeEditor -> IO CodeEditor
create oid model = do
    codeEditor <- create' (fromWidgetId oid) (model ^. Model.size . x) (model ^. Model.size . y)
    -- TODO[react]: Does not make sense anymore
    -- UI.setWidgetPosition (model ^. widgetPosition) codeEditor
    setCode model codeEditor
    return codeEditor

setCode :: Model.CodeEditor -> CodeEditor -> IO ()
setCode model codeEditor = setCode' codeEditor $ textToJSString $ model ^. Model.value

instance UIDisplayObject Model.CodeEditor where
    createUI parentId wid model = do
        codeEditor <- create wid model
        parent     <- UI.lookup parentId :: IO Widget.GenericWidget
        UI.register wid codeEditor
        Widget.add codeEditor parent

    updateUI wid old model = do
        codeEditor <- UI.lookup wid :: IO CodeEditor
        whenChanged old model Model.value $ setCode  model codeEditor

instance CompositeWidget Model.CodeEditor
instance ResizableWidget Model.CodeEditor where
    resizeWidget = UI.defaultResize
