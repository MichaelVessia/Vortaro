module Vortaro where

translate :: String -> IO ()
translate word = do
    mapM_ print [word, word, word]
