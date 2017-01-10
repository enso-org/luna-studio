{-# LANGUAGE OverloadedStrings #-}

module JS.GoogleAnalytics
    ( sendEvent
    , Event(..)
    , AddNodeType (..)
    , ConnectType (..)
    ) where

import           Data.JSString.Text        (lazyTextToJSString)
import           Data.Text.Lazy            (pack)
import           GHCJS.Nullable            (Nullable, maybeToNullable)
import           Luna.Studio.Prelude

import           Luna.Studio.Action.Command (Command, performIO)

data ConnectType = Manual
                 | Pen
                 deriving (Show)

data AddNodeType = Simple
                 | AutoConnect
                 deriving (Show)

data Event = BSOD Text -- sent directly from JS
           | ConnectionLost -- sent directly from JS4
           | AddNode AddNodeType
           | RemoveNode Int
           | Connect ConnectType
           | Disconnect
           | NodeSearcher
           | CommandSearcher
           | CreateProject
           | SwitchProject
           | GAOptOut Bool
           | OpenHelp
           | ToggleText

data GAEvent = GAEvent { _category :: Text
                       , _action   :: Text
                       , _label    :: Maybe Text
                       , _value    :: Maybe Int
                       }

simpleEvent :: Text -> Text -> GAEvent
simpleEvent c a = GAEvent c a Nothing Nothing

toGAEvent :: Event -> GAEvent
toGAEvent ev = case ev of
    BSOD message        -> GAEvent     "Diagnostic"      "BSOD"           (Just message) Nothing
    ConnectionLost      -> simpleEvent "Diagnostic"      "ConnectionLost"
    AddNode tpe         -> GAEvent     "Graph"           "AddNode"        (Just $ pack $ show tpe) Nothing
    RemoveNode n        -> GAEvent     "Graph"           "RemoveNode"     (Just $ pack $ show n)   Nothing
    Connect tpe         -> GAEvent     "Graph"           "Connect"        (Just $ pack $ show tpe) Nothing
    Disconnect          -> simpleEvent "Graph"           "Disconnect"
    NodeSearcher        -> simpleEvent "NodeSearcher"    "Open"
    CommandSearcher     -> simpleEvent "CommandSearcher" "Open"
    CreateProject       -> simpleEvent "Project"         "Create"
    SwitchProject       -> simpleEvent "Project"         "Switch"
    GAOptOut s          -> GAEvent     "Settings"        "GAOptOut"       (Just $ pack $ show s)  Nothing
    OpenHelp            -> simpleEvent "UI"              "OpenHelp"
    ToggleText          -> simpleEvent "UI"              "ToggleText"

foreign import javascript safe "ga('send', 'event', $1, $2, $3)" sendEvent' :: JSString -> JSString -> Nullable JSString -> Nullable Int -> IO ()

sendEvent :: Event -> Command a ()
sendEvent event = performIO $ sendEvent' cat' act' lab' val' where
        GAEvent cat act lab val = toGAEvent event
        cat' = lazyTextToJSString cat
        act' = lazyTextToJSString act
        lab' = maybeToNullable $ lazyTextToJSString <$> lab
        val' = maybeToNullable val
