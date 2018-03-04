{-# LANGUAGE OverloadedStrings #-}
module Main where

import Vortaro
import System.IO
import Data.Text
import Data.Text.IO as TIO

doTranslate :: IO ()
doTranslate = do
    TIO.putStrLn "\n\n"
    TIO.putStrLn "Please enter a word"
    word <- TIO.getLine
    withFile "espdic/espdic.txt" ReadMode $ \fileHandle -> do
        contents <- TIO.hGetContents fileHandle
        translate word contents
        main

main :: IO ()
main = do
  TIO.putStrLn "\n\n"
  TIO.putStrLn "What would you like to do?"
  TIO.putStrLn "1 Translate a word"
  TIO.putStrLn "2 Quit"
  action <- TIO.getLine
  case action of
    "1" -> doTranslate
    "2" -> return ()
    otherwise -> main

