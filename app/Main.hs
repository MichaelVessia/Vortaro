module Main where

import Vortaro
import System.IO

main :: IO ()
main = do
    putStrLn "Please enter an English word."
    word <- getLine
    withFile "espdic/espdic.txt" ReadMode $ \fileHandle -> do
        contents <- hGetContents fileHandle
        translate word contents
