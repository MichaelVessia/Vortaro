module Vortaro where

import Data.List (isInfixOf)
import Data.Char (toLower)

-- An English word
type EnWord = String

-- An Esperanto word, found on the LHS of a dictionary entry
data EoWord = EoWord String deriving (Show, Eq)

-- A definition of an Esperanto word, found on the RHS of a dictionary entry
data Definition = Definition String deriving (Show, Eq)

-- Product type representing a single line in the dictionary
data Entry = Entry EoWord Definition deriving (Show, Eq)

translate :: EnWord -> String -> IO ()
translate word rawDic =
    if translations == [] then putStrLn "No translation found" else mapM_ (putStrLn . show) translations
        where translations = mkEntries (searchForWord (makeLowerCase word) (formatAllLines . getLines $ rawDic))

-- Return lines in the espdic for which the given EnWord can be found
searchForWord :: EnWord -> [String] -> [String]
searchForWord word espdic = filter (isInfixOf word) espdic

definitionContainsWord :: EnWord -> Entry -> Bool
definitionContainsWord word (Entry _ (Definition def)) = isInfixOf word def

-- Format the EspDic input String prior to its transformation into the real type
format :: String -> String
format = makeLowerCase . stripQuotes

formatAllLines :: [String] -> [String]
formatAllLines = map format

getLines :: String -> [String]
getLines = tail . lines

-- Dictionary entries are split by a :
-- Construct an Entry for a given line
mkEntry :: String -> Entry
mkEntry line = (Entry (EoWord left) (Definition right))
    where left = parts !! 0
          right = parts !! 1
          parts = split line ':'

mkEntries :: [String] -> [Entry]
mkEntries = map mkEntry

split :: String -> Char -> [String]
split [] delim = [""]
split (x:xs) delim
    | x == delim = "" : rest
    | otherwise = (x : head rest) : tail rest
    where rest = split xs delim

makeLowerCase :: String -> String
makeLowerCase = map toLower

stripQuotes :: String -> String
stripQuotes = filter (/='"')
