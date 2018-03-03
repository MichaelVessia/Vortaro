{-# LANGUAGE OverloadedStrings #-}
module Main where

import Vortaro
import System.IO
import Data.Text.IO as TIO

main :: IO ()
main = do
    TIO.putStrLn "Please enter a word"
    word <- TIO.getLine
    withFile "espdic/espdic.txt" ReadMode $ \fileHandle -> do
        contents <- TIO.hGetContents fileHandle
        translate word contents
