{-# LANGUAGE OverloadedStrings #-}
module Main where

import Vortaro
import System.IO
import System.Exit
import Data.Text
import Data.Text.IO as TIO

doTranslate :: IO ()
doTranslate = do
    TIO.putStrLn "\n"
    TIO.putStrLn "Please enter a word. Enter Q to quit."
    word <- TIO.getLine
    withFile "espdic/espdic.txt" ReadMode $ \fileHandle -> do
        contents <- TIO.hGetContents fileHandle
        case word of
          "Q" -> exitWith ExitSuccess
          otherwise -> translate word contents
        main

main :: IO ()
main = doTranslate


