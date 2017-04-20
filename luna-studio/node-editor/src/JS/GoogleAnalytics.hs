{-# LANGUAGE OverloadedStrings #-}

module JS.GoogleAnalytics
    ( sendEvent
    , Event(..)
    , AddNodeType (..)
    , ConnectType (..)
    ) where

import           GHCJS.Nullable             (Nullable)
import           Common.Prelude

import           NodeEditor.Action.Command (Command)

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
    AddNode tpe         -> GAEvent     "Graph"           "AddNode"        (Just $ convert $ show tpe) Nothing
    RemoveNode n        -> GAEvent     "Graph"           "RemoveNode"     (Just $ convert $ show n)   Nothing
    Connect tpe         -> GAEvent     "Graph"           "Connect"        (Just $ convert $ show tpe) Nothing
    Disconnect          -> simpleEvent "Graph"           "Disconnect"
    NodeSearcher        -> simpleEvent "NodeSearcher"    "Open"
    CommandSearcher     -> simpleEvent "CommandSearcher" "Open"
    CreateProject       -> simpleEvent "Project"         "Create"
    SwitchProject       -> simpleEvent "Project"         "Switch"
    GAOptOut s          -> GAEvent     "Settings"        "GAOptOut"       (Just $ convert $ show s)  Nothing
    ToggleText          -> simpleEvent "UI"              "ToggleText"

foreign import javascript safe "ga('send', 'event', $1, $2, $3)" sendEvent' :: JSString -> JSString -> Nullable JSString -> Nullable Int -> IO ()

sendEvent :: Event -> Command a ()
sendEvent = const $ return () --TODO it appears to conflict with Atom
-- sendEvent event = liftIO $ sendEvent' cat' act' lab' val' where
--         GAEvent cat act lab val = toGAEvent event
--         cat' = textToJSString cat
--         act' = textToJSString act
--         lab' = maybeToNullable $ textToJSString <$> lab
--         val' = maybeToNullable val
