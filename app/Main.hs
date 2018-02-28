module Main where

import Vortaro

main :: IO ()
main = do
    input <- getLine
    translate input
