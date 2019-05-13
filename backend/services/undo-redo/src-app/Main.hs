module Main where

import Prologue

import qualified Bus.Data.Config as Config
import qualified Undo

import GHC.IO.Encoding (setLocaleEncoding, utf8)

main :: IO ()
main = do
    setLocaleEncoding utf8
    endPoints <- Config.readDefault
    r <- Undo.run endPoints
    return ()
