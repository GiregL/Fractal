module Main where

import Parser

import System.IO
import System.Exit
import System.Environment
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    code <- readFile $ head args
    print (result code)