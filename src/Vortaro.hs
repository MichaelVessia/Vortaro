module Vortaro where

import Data.List (isInfixOf)
import Data.Char (toLower)

-- An English word
type EnWord = String
-- The Esperanto dictionary file
type EspDic = String

data EoWord = EoWord String

data Definition = Definition String

data Entry = Entry EoWord Definition

translate :: EnWord -> EspDic -> IO ()
translate word espdic = do
    if translations == [] then putStrLn "No translation found" else mapM_ putStrLn translations
        where translations = (searchForWord (makeLowerCase word) espdic)

-- Return lines in the espdic for which the given EnWord can be found
searchForWord :: EnWord -> EspDic -> [String]
searchForWord word espdic = filter (isInfixOf word) (format espdic)

format :: EspDic -> [String]
format = tail . lines . makeLowerCase . stripQuotes

makeLowerCase :: String -> String
makeLowerCase = map toLower

stripQuotes :: String -> String
stripQuotes = filter (/='"')

mkEntry :: String -> Entry
mkEntry line = undefined

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
    | x == delim = "" : rest
    | otherwise = (x : head rest) : tail rest
    where rest = split xs delim

