{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad                  (forM_, when)
import           Control.Monad.State            (StateT, evalStateT, get, liftIO, modify, put)
import           Data.Default
import           Data.Monoid                    ((<>))
import           Data.Text                      (Text)
import qualified Data.Text                      as Text
import           System.Console.ANSI
import           System.IO
import           Text.Show.Pretty

import           Text.ScopeSearcher.Item
import           Text.ScopeSearcher.QueryResult
import qualified Text.ScopeSearcher.Scope       as Scope
import qualified Text.ScopeSearcher.Searcher    as Searcher

import qualified Mock.Mix                       as Mock

data Env = Env { _debug       :: Bool
               , _includePath :: Bool
               } deriving (Show)

instance Default Env where
    def = Env True False

makeLenses ''Env

lineWidth     = 54
moduleWidth   = 18
functionWidth = 22

main :: IO ()
main = do
    withColor Vivid Magenta putStrLn "ScopeSearcher test application"
    showLine
    showHelp
    showLine
    evalStateT showHighlights def
    withColor Vivid Red putStrLn "Leaving ScopeSearcher."

showHelp :: IO ()
showHelp =  withColor Vivid Yellow  putStrLn ":h      - show help\n:m      - show mock\n:d      - toggle debug\n[enter] - quit"

showMock :: IO ()
showMock = withColor Vivid Green putStrLn $ ppShow Mock.group

showHighlights :: StateT Env IO ()
showHighlights = do
    input <- liftIO $ getUserInput
    when (not $ null input) $ do
        let shouldToggleDebug       = take 2 input == ":d"
            shouldShowMock          = take 2 input == ":m"
            shouldShowHelp          = take 2 input == ":h"
        debugState       <- use debug
        case (shouldToggleDebug, shouldShowMock, shouldShowHelp) of
            (False, False, False) -> liftIO $ showQueryResults debugState $ Text.pack input
            (_, _, _) -> do
                when shouldToggleDebug       updateDebug
                when shouldShowMock $ liftIO showMock
                when shouldShowHelp $ liftIO showHelp
        showHighlights

updateDebug :: StateT Env IO ()
updateDebug = zoom debug $ do
    modify not
    debug <- get
    liftIO $ withColor Vivid Red putStrLn $ "Toggled debug to " <> show debug

updateIncludePath :: StateT Env IO ()
updateIncludePath = zoom includePath $ do
    modify not
    includePath <- get
    liftIO $ withColor Vivid Red putStrLn $ "Toggled includePath to " <> show includePath

getUserInput :: IO String
getUserInput = do
    withColor Vivid Blue putStr "> "
    hFlush stdout
    getLine

showLine :: IO ()
showLine = withColor Dull Green putStrLn $ replicate lineWidth '-'

fillLeft, fillRight :: Int -> String -> String
fillLeft  n s = s <> replicate (n - length s) ' '
fillRight n s = replicate (n - length s) ' ' <> s

showQueryResults :: Bool -> Text -> IO ()
showQueryResults debug searchText = do
    let suggestions = Scope.searchInScope Mock.items searchText
    when debug $ withColor Dull Yellow putStrLn $ ppShow suggestions
    showLine
    forM_ suggestions showQueryResult
    showLine

showQueryResult :: QueryResult -> IO ()
showQueryResult (QueryResult prefix name fullname highlights tpe score) = do
    let nameString = Text.unpack name
    -- let nameString = Text.unpack fullname
    withColor Dull Blue putStr $ fillRight moduleWidth $ (Text.unpack prefix) <> " "
    showHighlight 0 highlights nameString
    putStr $ replicate (functionWidth - length nameString) ' '
    putStrLn $ Text.unpack tpe

showHighlight :: Int -> [Highlight] -> String -> IO ()
showHighlight _ [] str = withColor Dull White putStr str
showHighlight pos ((Highlight start len):highlights) str = do
    let hlPos = start - pos
        (normal, remainder) = splitAt hlPos str
        (highlighted, rest) = splitAt len remainder
    withColor Dull  White putStr normal
    withColor Vivid Red   putStr highlighted
    showHighlight (hlPos + len) highlights rest

withColor :: Show a => ColorIntensity -> Color -> (a -> IO ()) -> a -> IO ()
withColor colorIntensity color printFun output = do
    setSGR [SetColor Foreground colorIntensity color]
    printFun output
    setSGR [SetColor Foreground Dull White]
