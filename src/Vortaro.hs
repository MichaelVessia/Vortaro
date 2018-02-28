module Vortaro where

import Data.List (isInfixOf)
import Data.Char (toLower)

-- An English word
type EnWord = String
-- The Esperanto dictionary file
type EspDic = String

translate :: EnWord -> EspDic -> IO ()
translate word espdic = do
    if translations == [] then putStrLn "No translation found" else mapM_ print translations
        where translations = (searchForWord (makeLowerCase word) espdic)

-- Return lines in the espdic for which the given EnWord can be found
searchForWord :: EnWord -> EspDic -> [String]
searchForWord word espdic = filter (isInfixOf word) (tail . lines . makeLowerCase $ espdic)

makeLowerCase :: String -> String
makeLowerCase = map toLower
